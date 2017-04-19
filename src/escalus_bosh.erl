%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module abstracting TCP connection to XMPP server
%%% @end
%%%===================================================================

-module(escalus_bosh).
-behaviour(gen_server).
-behaviour(escalus_connection).

-include_lib("exml/include/exml_stream.hrl").
-include("escalus.hrl").
-include("escalus_xmlns.hrl").

%% Escalus transport callbacks
-export([connect/1,
         send/2,
         is_connected/1,
         upgrade_to_tls/2,
         use_zlib/2,
         get_transport/1,
         reset_parser/1,
         stop/1,
         kill/1,
         set_filter_predicate/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% BOSH XML elements
-export([session_creation_body/2, session_creation_body/6,
         session_termination_body/2,
         empty_body/2, empty_body/3]).

%% Low level API
-export([send_raw/2,
         resend_raw/2,
         get_sid/1,
         get_rid/1,
         get_keepalive/1,
         set_keepalive/2,
         mark_as_terminated/1,
         pause/2,
         get_active/1,
         set_active/2,
         recv/1,
         get_requests/1,
         set_quickfail/2]).

-define(WAIT_FOR_SOCKET_CLOSE_TIMEOUT, 200).
-define(SERVER, ?MODULE).
-define(DEFAULT_WAIT, 60).
-define(MAX_CONCURRENT_REQUESTS, 2).

-record(state, {
          owner,
          url,
          parser,
          sid = nil,
          rid = nil,
          pending_requests,
          requests,
          pending_replies = [],
          waiting_requesters = [],
          keepalive = true,
          wait,
          active = true,
          replies = [],
          terminated = false,
          event_client,
          client,
          on_reply,
          filter_pred,
          quickfail = false
         }).

-type state() :: #state{}.
-type async_req() :: {reference(), fun(() -> any())}.

%%%===================================================================
%%% API
%%%===================================================================

-spec connect([{atom(), any()}]) -> {ok, escalus:client()}.
connect(Args) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Args, self()], []),
    Transport = gen_server:call(Pid, get_transport),
    {ok, Transport}.

send(#client{rcv_pid = Pid} = Socket, Elem) ->
    gen_server:call(Pid, {send, Socket, Elem}).

is_connected(#client{rcv_pid = Pid}) ->
    erlang:is_process_alive(Pid).

reset_parser(#client{rcv_pid = Pid}) ->
    gen_server:cast(Pid, reset_parser).

stop(#client{rcv_pid = Pid}) ->
    try
        gen_server:call(Pid, stop)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            already_stopped;
        exit:{normal, {gen_server, call, _}} ->
            already_stopped;
        exit:{timeout, {gen_server, call, _}} ->
            error({timeout, process_info(Pid, current_stacktrace),
                   process_info(Pid, messages), catch sys:get_state(Pid)})
    end.

kill(#client{} = Client) ->
    mark_as_terminated(Client),
    stop(Client).

upgrade_to_tls(#client{} = _Conn, _Props) ->
    not_supported.

use_zlib(#client{} = _Conn, _Props) ->
    not_supported.

get_transport(#client{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_transport).

-spec set_filter_predicate(escalus_connection:client(),
    escalus_connection:filter_pred()) -> ok.
set_filter_predicate(#client{rcv_pid = Pid}, Pred) ->
    gen_server:call(Pid, {set_filter_pred, Pred}).

%%%===================================================================
%%% BOSH XML elements
%%%===================================================================

session_creation_body(Rid, To) ->
    session_creation_body(?DEFAULT_WAIT, <<"1.0">>, <<"en">>, Rid, To, nil).

session_creation_body(Wait, Version, Lang, Rid, To, nil) ->
    empty_body(Rid, nil,
               [{<<"content">>, <<"text/xml; charset=utf-8">>},
                {<<"xmlns:xmpp">>, ?NS_BOSH},
                {<<"xmpp:version">>, Version},
                {<<"ver">>, <<"1.6">>},
                {<<"hold">>, <<"1">>},
                {<<"wait">>, list_to_binary(integer_to_list(Wait))},
                {<<"xml:lang">>, Lang},
                {<<"to">>, To}]);

session_creation_body(_Wait, _Version, Lang, Rid, To, Sid) ->
    empty_body(Rid, Sid,
                [{<<"xmlns:xmpp">>, ?NS_BOSH},
                 {<<"xml:lang">>, Lang},
                 {<<"to">>, To},
                 {<<"xmpp:restart">>, <<"true">>}]).

session_termination_body(Rid, Sid) ->
    Body = empty_body(Rid, Sid, [{<<"type">>, <<"terminate">>}]),
    Body#xmlel{children = [escalus_stanza:presence(<<"unavailable">>)]}.

empty_body(Rid, Sid) ->
    empty_body(Rid, Sid, []).

empty_body(Rid, Sid, ExtraAttrs) ->
    #xmlel{name = <<"body">>,
           attrs = common_attrs(Rid, Sid) ++ ExtraAttrs}.

pause_body(Rid, Sid, Seconds) ->
    Empty = empty_body(Rid, Sid),
    Pause = {<<"pause">>, integer_to_binary(Seconds)},
    Empty#xmlel{attrs = Empty#xmlel.attrs ++ [Pause]}.

common_attrs(Rid) ->
    [{<<"rid">>, pack_rid(Rid)},
     {<<"xmlns">>, ?NS_HTTP_BIND}].

common_attrs(Rid, nil) ->
    common_attrs(Rid);
common_attrs(Rid, Sid) ->
    common_attrs(Rid) ++ [{<<"sid">>, Sid}].

pack_rid(Rid) ->
    integer_to_binary(Rid).

%%%===================================================================
%%% Low level API
%%%===================================================================

%% Watch out for request IDs!
%%
%% In general, you should not use this function,
%% as this transport (i.e. escalus_bosh) takes care
%% of wrapping ordinary XMPP stanzas for you.
%%
%% However, in case of the need for a low-level access interleaving
%% calls to send/2 and send_raw/2 is tricky.
%% For send/2 the transport keeps track of an internal
%% request ID which might not necessarily be consistent with the one supplied
%% when manually building the BOSH body and sending it with send_raw/2.
%% Always use get_rid/1 which will give you a valid request ID to use
%% when manually wrapping stanzas to send_raw/2.
%%
%% Otherwise, the non-matching request IDs will
%% confuse the server and possibly cause errors.
send_raw(#client{rcv_pid = Pid} = Transport, Body) ->
    gen_server:cast(Pid, {send_raw, Transport, Body}).

%% This is much like send_raw/2 except for the fact that
%% the request ID won't be autoincremented on send.
%% I.e. it is intended for resending packets which were
%% already sent.
resend_raw(#client{rcv_pid = Pid} = Transport, Body) ->
    gen_server:cast(Pid, {resend_raw, Transport, Body}).

get_rid(#client{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_rid).

get_sid(#client{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_sid).

get_keepalive(#client{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_keepalive).

set_keepalive(#client{rcv_pid = Pid}, NewKeepalive) ->
    gen_server:call(Pid, {set_keepalive, NewKeepalive}).

mark_as_terminated(#client{rcv_pid = Pid}) ->
    gen_server:call(Pid, mark_as_terminated).

pause(#client{rcv_pid = Pid} = Transport, Seconds) ->
    gen_server:cast(Pid, {pause, Transport, Seconds}).

%% get_-/set_active tries to tap into the intuition gained from using
%% inet socket option {active, true | false | once}.
%% An active BOSH transport sends unpacked stanzas to an escalus client,
%% where they can be received using wait_for_stanzas.
%% An inactive BOSH transport buffers the stanzas in its state.
%% They can be retrieved using escalus_bosh:recv.
%%
%% Sometimes it's necessary to intercept the whole BOSH wrapper
%% not only the wrapped stanzas. That's when this mechanism proves useful.
get_active(#client{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_active).

set_active(#client{rcv_pid = Pid}, Active) ->
    gen_server:call(Pid, {set_active, Active}).

-spec recv(escalus:client()) -> exml_stream:element() | empty.
recv(#client{rcv_pid = Pid}) ->
    gen_server:call(Pid, recv).

get_requests(#client{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_requests).

%% This flag makes client to fail on stream error,
%% even if it arrives out of order (according to RIDs)
set_quickfail(#client{rcv_pid = Pid}, QuickfailFlag) ->
    gen_server:call(Pid, {set_quickfail, QuickfailFlag}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% TODO: refactor all opt defaults taken from Args into a default_opts function,
%%       so that we know what options the module actually expects
init([Args, Owner]) ->
    Host = proplists:get_value(host, Args, <<"localhost">>),
    Port = proplists:get_value(port, Args, 5280),
    Path = proplists:get_value(path, Args, <<"/http-bind">>),
    Wait = proplists:get_value(bosh_wait, Args, ?DEFAULT_WAIT),
    HTTPS = proplists:get_value(ssl, Args, false),
    EventClient = proplists:get_value(event_client, Args),
    HostStr = host_to_list(Host),
    OnReplyFun = proplists:get_value(on_reply, Args, fun(_) -> ok end),
    OnConnectFun = proplists:get_value(on_connect, Args, fun(_) -> ok end),
    {MS, S, MMS} = now(),
    InitRid = MS * 1000000 * 1000000 + S * 1000000 + MMS,
    {ok, Parser} = exml_stream:new_parser(),
    {ok, Client} = fusco_cp:start_link({HostStr, Port, HTTPS},
                                       [{on_connect, OnConnectFun}],
                                       %% Max two connections as per BOSH rfc
                                       2),
    {ok, #state{owner = Owner,
                url = Path,
                parser = Parser,
                rid = InitRid,
                keepalive = proplists:get_value(keepalive, Args, true),
                wait = Wait,
                requests = queue:new(),
                pending_requests = queue:new(),
                event_client = EventClient,
                client = Client,
                on_reply = OnReplyFun}}.


handle_call({send, Transport, Elem}, _From, State) ->
    NewState = wrap_and_send(Transport, Elem, State),
    {reply, ok, NewState};

handle_call(get_transport, _From, State) ->
    {reply, transport(State), State};

handle_call(get_sid, _From, #state{sid = Sid} = State) ->
    {reply, Sid, State};

handle_call(get_rid, _From, #state{rid = Rid} = State) ->
    {reply, Rid, State};

handle_call(get_keepalive, _From, #state{keepalive = Keepalive} = State) ->
    {reply, Keepalive, State};
handle_call({set_keepalive, NewKeepalive}, _From,
            #state{keepalive = Keepalive} = State) ->
    {reply, {ok, Keepalive, NewKeepalive},
     State#state{keepalive = NewKeepalive}};

handle_call(mark_as_terminated, _From, #state{} = State) ->
    {reply, {ok, marked_as_terminated}, State#state{terminated = true}};

handle_call(get_active, _From, #state{active = Active} = State) ->
    {reply, Active, State};
handle_call({set_active, Active}, _From, State) ->
    {reply, ok, State#state{active = Active}};

handle_call(recv, _From, State) ->
    {Reply, NS} = handle_recv(State),
    {reply, Reply, NS};

handle_call(get_requests, _From, State) ->
    {reply, queue:len(State#state.requests) + queue:len(State#state.pending_requests), State};

handle_call({set_filter_pred, Pred}, _From, State) ->
    {reply, ok, State#state{filter_pred = Pred}};

handle_call({set_quickfail, QuickfailFlag}, _From, State) ->
    {reply, ok, State#state{quickfail = QuickfailFlag}};

handle_call(stop, _From, #state{ terminated = true } = State) ->
    {stop, normal, ok, State};
handle_call(stop, From, #state{ waiting_requesters = WaitingRequesters } = State) ->
    StreamEnd = escalus_stanza:stream_end(),
    Ref = make_ref(),
    NewState = wrap_and_send(transport(State), exml:to_iolist(StreamEnd), Ref, State),
    {noreply, NewState#state{ waiting_requesters = [{Ref, From} | WaitingRequesters] }}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({send_raw, Transport, Body}, State) ->
    NewState = send(Transport, Body, State),
    {noreply, NewState};
handle_cast({resend_raw, Transport, Body}, State) ->
    NewState = send(Transport, Body, make_ref(), State#state.rid, State),
    {noreply, NewState};
handle_cast({pause, Transport, Seconds},
            #state{rid = Rid, sid = Sid} = State) ->
    NewState = send(Transport, pause_body(Rid, Sid, Seconds), State),
    {noreply, NewState};
handle_cast(reset_parser, #state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {noreply, State#state{parser = NewParser}}.


%% Handle async HTTP request replies.
handle_info(_, #state{ terminated = true } = S) ->
    {noreply, S};
handle_info({http_reply, Ref, Body, Transport} = HttpReply,
            #state{ pending_replies = PendingReplies } = S0) ->
    {ok, #xmlel{attrs = Attrs} = XmlBody} = exml:parse(Body),
    NewS = case {queue:peek(S0#state.requests),
                 S0#state.quickfail andalso detect_type(Attrs) == streamend} of
               {_, true} ->
                   S1 = handle_http_reply(Ref, XmlBody, Transport, S0),
                   S1#state{ pending_replies = [] };
               {{value, {Ref, _Rid, _Pid}}, _} ->
                   {{value, {Ref, _Rid, _Pid}}, NewRequests} = queue:out(S0#state.requests),
                   S1 = handle_http_reply(Ref, XmlBody, Transport,
                                          S0#state{ requests = NewRequests }),
                   lists:foreach(fun(PendingReply) -> self() ! PendingReply end,
                                 S1#state.pending_replies),
                   S1#state{ pending_replies = [] };
               _ ->
                   S0#state{ pending_replies = [HttpReply | PendingReplies] }
           end,
    {noreply, NewS};
handle_info(_, State) ->
    {noreply, State}.


terminate(_Reason, #state{client = Client, parser = Parser}) ->
    fusco_cp:stop(Client),
    exml_stream:free_parser(Parser).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Helpers
%%%===================================================================

request(#client{socket = {Client, Path}}, Body, OnReplyFun) ->
    Headers = [{<<"Content-Type">>, <<"text/xml; charset=utf-8">>}],
    BodyIO = exml:to_iolist(Body),
    Reply = fusco_cp:request(Client, Path, "POST", Headers, BodyIO, 2, infinity),
    OnReplyFun(Reply),
    {ok, {_Status, _Headers, RBody, _Size, _Time}} = Reply,
    {ok, RBody}.

close_requests(#state{requests = Reqs} = S) ->
    [exit(Pid, normal) || {_Ref, _Rid, Pid} <- queue:to_list(Reqs)],
    S#state{requests = queue:new(), pending_requests = queue:new()}.

wrap_and_send(Transport, Elem, State) ->
    wrap_and_send(Transport, Elem, make_ref(), State).

wrap_and_send(Transport, Elem, Ref, State) ->
    send(Transport, wrap_elem(Elem, State), Ref, State).

send(Transport, Body, State) ->
    send(Transport, Body, make_ref(), State).

send(Transport, Body, Ref, State) ->
    send(Transport, Body, Ref, State#state.rid + 1, State).

send(_Transport, _Body, _Ref, _NewRid, #state{ terminated = true } = S) ->
    %% Sending anything to a terminated session is pointless.
    %% We leave it in its current state to pick up any pending replies.
    S;
send(Transport, Body, Ref, NewRid, #state{ on_reply = OnReplyFun } = State) ->
    AsyncReq = prep_request(Transport, Body, OnReplyFun, Ref),
    start_request_or_enqueue(AsyncReq, State#state{ rid = NewRid }).

prep_request(Transport, Body, OnReplyFun, Ref) ->
    Self = self(),
    % Call to send_raw may lead to this function, so we can't trust Rid from State,
    % so we extract it from Body here, since this is the Rid the server will see
    Rid = binary_to_integer(exml_query:attr(Body, <<"rid">>)),
    {Ref, Rid,
     fun() ->
             {ok, Reply} = request(Transport, Body, OnReplyFun),
             Self ! {http_reply, Ref, Reply, Transport}
     end}.

start_request_or_enqueue(AsyncReq, #state{ requests = Requests,
                                           pending_requests = PendingRequests } = State) ->
    case queue:len(Requests) >= ?MAX_CONCURRENT_REQUESTS of
        true ->
            State#state{ pending_requests = queue_insert_by_rid(AsyncReq, PendingRequests) };
        false ->
            start_async_request(AsyncReq, State)
    end.

-spec start_async_request(async_req(), state()) -> state().
start_async_request({Ref, Rid, ReqFun}, #state{ requests = Requests } = State) ->
    NewRequests = queue_insert_by_rid({Ref, Rid, proc_lib:spawn(ReqFun)}, Requests),
    State#state{ requests = NewRequests }.

handle_http_reply(Ref, #xmlel{ attrs = Attrs } = XmlBody, Transport, #state{} = S1) ->
    S2 = case queue:out(S1#state.pending_requests) of
             {empty, _} ->
                 S1;
             {{value, NextRequest}, NewPendingRequests} ->
                 start_async_request(NextRequest, S1#state{ pending_requests = NewPendingRequests })
         end,
    S3 = handle_data(XmlBody, S2),
    S4 = case {detect_type(Attrs), S3#state.keepalive, queue:len(S3#state.requests) == 0} of
              {streamend, _, _} -> close_requests(S3#state{terminated = true});
              {_, false, _}     -> S3;
              {_, true, true}   -> send(Transport, empty_body(S3#state.rid, S3#state.sid), S3);
              {_, true, false}  -> S3
          end,
    case lists:keytake(Ref, 1, S4#state.waiting_requesters) of
        {value, {_, RequesterPid}, NewWaitingRequesters} ->
            gen_server:reply(RequesterPid, ok),
            S4#state{ waiting_requesters = NewWaitingRequesters };
        false ->
            S4
    end.

handle_data(#xmlel{} = Body, #state{} = State) ->
    NewState = case State#state.sid of
        %% First reply for this transport, set sid
        nil ->
            State#state{sid = exml_query:attr(Body, <<"sid">>)};
        _ ->
            State
    end,
    Stanzas = unwrap_elem(Body),
    case State#state.active of
        true ->
            escalus_connection:maybe_forward_to_owner(NewState#state.filter_pred,
                                                      NewState, Stanzas,
                                                      fun forward_to_owner/2),
            NewState;
        false ->
            store_reply(Body, NewState)
    end.

forward_to_owner(Stanzas, #state{owner = Owner,
                                 event_client = EventClient} = S) ->
    lists:foreach(fun(Stanza) ->
        escalus_event:incoming_stanza(EventClient, Stanza),
        Owner ! {stanza, transport(S), Stanza}
    end, Stanzas),
    case lists:keyfind(xmlstreamend, 1, Stanzas) of
        false -> ok;
        _ -> gen_server:cast(self(), stop)
    end.

store_reply(Body, #state{replies = Replies} = S) ->
    S#state{replies = Replies ++ [Body]}.

handle_recv(#state{replies = []} = S) ->
    {empty, S};
handle_recv(#state{replies = [Reply | Replies]} = S) ->
    case Reply of
        #xmlstreamend{} ->
            gen_server:cast(self(), stop);
        _ -> ok
    end,
    {Reply, S#state{replies = Replies}}.

transport(#state{url = Path, client = Client, event_client = EventClient}) ->
    #client{module = ?MODULE,
               ssl = false,
               compress = false,
               rcv_pid = self(),
               socket = {Client, Path},
               event_client = EventClient}.

wrap_elem(#xmlstreamstart{attrs = Attrs},
          #state{rid = Rid, sid = Sid, wait = Wait}) ->
    Version = proplists:get_value(<<"version">>, Attrs, <<"1.0">>),
    Lang = proplists:get_value(<<"xml:lang">>, Attrs, <<"en">>),
    To = proplists:get_value(<<"to">>, Attrs, <<"localhost">>),
    session_creation_body(Wait, Version, Lang, Rid, To, Sid);
wrap_elem(["</", <<"stream:stream">>, ">"], #state{sid=Sid, rid=Rid}) ->
    session_termination_body(Rid, Sid);
wrap_elem(Element, #state{sid = Sid, rid=Rid}) ->
    (empty_body(Rid, Sid))#xmlel{children = [Element]}.

unwrap_elem(#xmlel{name = <<"body">>, children = Body, attrs=Attrs}) ->
    Type = detect_type(Attrs),
    case Type of
        {streamstart, Ver} ->
            Server = proplists:get_value(<<"from">>, Attrs),
            StreamStart = #xmlstreamstart{name = <<"stream:stream">>, attrs=[
                        {<<"from">>, Server},
                        {<<"version">>, Ver},
                        {<<"xml:lang">>, <<"en">>},
                        {<<"xmlns">>, <<"jabber:client">>},
                        {<<"xmlns:stream">>,
                         <<"http://etherx.jabber.org/streams">>}]},
            [StreamStart];
        streamend ->
            [escalus_stanza:stream_end()];
        _ -> []
    end ++ Body.

detect_type(Attrs) ->
    Get = fun(A) -> proplists:get_value(A, Attrs) end,
    case {Get(<<"type">>), Get(<<"xmpp:version">>)} of
        {<<"terminate">>, _} -> streamend;
        {_, undefined} -> normal;
        {_, Version} -> {streamstart, Version}
    end.

host_to_list({_, _, _, _} = IP4) -> inet_parse:ntoa(IP4);
host_to_list({_, _, _, _, _, _, _, _} = IP6) -> inet_parse:ntoa(IP6);
host_to_list(BHost) when is_binary(BHost) -> binary_to_list(BHost);
host_to_list(Host) when is_list(Host) -> Host.

queue_insert_by_rid({_Ref, ReqRid, _} = Req, Queue) ->
    case queue:out(Queue) of
        {{value, {_, Rid, _} = Item}, Queue2} when Rid < ReqRid ->
            queue:in_r(Item, queue_insert_by_rid(Req, Queue2));
        {empty, _} ->
            queue:in(Req, Queue);
        _ ->
            queue:in_r(Req, Queue)
    end.

