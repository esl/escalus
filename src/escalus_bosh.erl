%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module abstracting TCP connection to XMPP server
%%% @end
%%%===================================================================

-module(escalus_bosh).
-behaviour(gen_server).

-include_lib("exml/include/exml_stream.hrl").
-include("include/escalus.hrl").
-include("include/escalus_xmlns.hrl").

%% Escalus transport callbacks
-export([connect/1,
         send/2,
         is_connected/1,
         upgrade_to_tls/2,
         use_zlib/2,
         get_transport/1,
         reset_parser/1,
         stop/1]).

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
         get_sid/1,
         get_rid/1,
         get_keepalive/1,
         set_keepalive/2,
         pause/2,
         get_active/1,
         set_active/2,
         recv/1,
         get_requests/1]).

-define(WAIT_FOR_SOCKET_CLOSE_TIMEOUT, 200).
-define(SERVER, ?MODULE).
-define(DEFAULT_WAIT, 60).

-record(state, {owner,
                url,
                parser,
                sid = nil,
                rid = nil,
                requests = [],
                keepalive = true,
                wait,
                active = true,
                replies = []}).

%%%===================================================================
%%% API
%%%===================================================================

-spec connect([{atom(), any()}]) -> {ok, #transport{}}.
connect(Args) ->
    ssl:start(),
    lhttpc:start(),
    {ok, Pid} = gen_server:start_link(?MODULE, [Args, self()], []),
    Transport = gen_server:call(Pid, get_transport),
    {ok, Transport}.

send(#transport{rcv_pid = Pid} = Socket, Elem) ->
    gen_server:cast(Pid, {send, Socket, Elem}).

is_connected(#transport{rcv_pid = Pid}) ->
    erlang:is_process_alive(Pid).

reset_parser(#transport{rcv_pid = Pid}) ->
    gen_server:cast(Pid, reset_parser).

stop(#transport{rcv_pid = Pid}) ->
    try
        gen_server:call(Pid, stop)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            already_stopped
    end.

upgrade_to_tls(#transport{} = _Conn, _Props) ->
    not_supported.

use_zlib(#transport{} = _Conn, _Props) ->
    not_supported.

get_transport(#transport{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_transport).

%%%===================================================================
%%% BOSH XML elements
%%%===================================================================

session_creation_body(Rid, To) ->
    session_creation_body(?DEFAULT_WAIT, <<"1.0">>, <<"en">>, Rid, To, nil).

session_creation_body(Wait, Version, Lang, Rid, To, Sid) ->
    empty_body(Rid, Sid,
               [{<<"content">>, <<"text/xml; charset=utf-8">>},
                {<<"xmlns:xmpp">>, ?NS_BOSH},
                {<<"xmpp:version">>, Version},
                {<<"hold">>, <<"1">>},
                {<<"wait">>, list_to_binary(integer_to_list(Wait))},
                {<<"xml:lang">>, Lang},
                {<<"to">>, To}]
               ++ [{<<"xmpp:restart">>, <<"true">>} || Sid =/= nil]).

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
    Pause = {<<"pause">>, list_to_binary(integer_to_list(Seconds))},
    Empty#xmlel{attrs = Empty#xmlel.attrs ++ [Pause]}.

common_attrs(Rid) ->
    [{<<"rid">>, pack_rid(Rid)},
     {<<"xmlns">>, ?NS_HTTP_BIND}].

common_attrs(Rid, nil) ->
    common_attrs(Rid);
common_attrs(Rid, Sid) ->
    common_attrs(Rid) ++ [{<<"sid">>, Sid}].

pack_rid(Rid) ->
    list_to_binary(integer_to_list(Rid)).

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
send_raw(#transport{rcv_pid = Pid} = Transport, Body) ->
    gen_server:cast(Pid, {send_raw, Transport, Body}).

get_rid(#transport{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_rid).

get_sid(#transport{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_sid).

get_keepalive(#transport{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_keepalive).

set_keepalive(#transport{rcv_pid = Pid}, NewKeepalive) ->
    gen_server:call(Pid, {set_keepalive, NewKeepalive}).

pause(#transport{rcv_pid = Pid} = Transport, Seconds) ->
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
get_active(#transport{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_active).

set_active(#transport{rcv_pid = Pid}, Active) ->
    gen_server:call(Pid, {set_active, Active}).

-spec recv(#transport{}) -> xmlstreamelement() | empty.
recv(#transport{rcv_pid = Pid}) ->
    gen_server:call(Pid, recv).

get_requests(#transport{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_requests).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Args, Owner]) ->
    Host = proplists:get_value(host, Args, <<"localhost">>),
    Port = proplists:get_value(port, Args, 5280),
    Path = proplists:get_value(path, Args, <<"/http-bind">>),
    Wait = proplists:get_value(bosh_wait, Args, ?DEFAULT_WAIT),
    HostStr = binary_to_list(Host),
    {MS, S, MMS} = now(),
    InitRid = MS * 1000000 * 1000000 + S * 1000000 + MMS,
    {ok, Parser} = exml_stream:new_parser(),
    {ok, #state{owner = Owner,
                url = {HostStr, Port, binary_to_list(Path)},
                parser = Parser,
                rid = InitRid,
                keepalive = proplists:get_value(keepalive, Args, true),
                wait = Wait}}.


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

handle_call(get_active, _From, #state{active = Active} = State) ->
    {reply, Active, State};
handle_call({set_active, Active}, _From, State) ->
    {reply, ok, State#state{active = Active}};

handle_call(recv, _From, State) ->
    {Reply, NS} = handle_recv(State),
    {reply, Reply, NS};

handle_call(get_requests, _From, State) ->
    {reply, length(State#state.requests), State};

handle_call(stop, _From, #state{} = State) ->
    StreamEnd = escalus_stanza:stream_end(),
    NewState = send0(transport(State), exml:to_iolist(StreamEnd), State),
    {stop, normal, ok, NewState}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({send, Transport, Elem}, State) ->
    NewState = send0(Transport, Elem, State),
    {noreply, NewState};
handle_cast({send_raw, Transport, Body}, State) ->
    NewState = send(Transport, Body, State),
    {noreply, NewState};
handle_cast({pause, Transport, Seconds},
            #state{rid = Rid, sid = Sid} = State) ->
    NewState = send(Transport, pause_body(Rid, Sid, Seconds), State),
    {noreply, NewState};
handle_cast(reset_parser, #state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {noreply, State#state{parser = NewParser}}.


%% Handle async HTTP request replies.
handle_info({http_reply, Ref, {_StatusAndReason, _Hdrs, Body},
             Transport}, S) ->
    NewRequests = lists:keydelete(Ref, 1, S#state.requests),
    NS = handle_data(Body, S#state{requests = NewRequests}),
    NNS = case {NS#state.keepalive, NS#state.requests == []} of
        {false, _} ->
            NS;
        {true, true} ->
            send(Transport, empty_body(NS#state.rid, NS#state.sid), NS);
        {true, false} ->
            NS
    end,
    {noreply, NNS};
handle_info(_, State) ->
    {noreply, State}.


terminate(_Reason, #state{parser = Parser}) ->
    exml_stream:free_parser(Parser).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Helpers
%%%===================================================================
send(#transport{socket = {Host, Port, Path}} = Transport, Body,
     #state{requests = Requests} = S) ->
    Headers = [{"Content-Type", "text/xml; charset=utf-8"}],
    Ref = make_ref(),
    Self = self(),
    AsyncReq = fun() ->
            {ok, Reply} = lhttpc:request(Host, Port, false, Path, 'POST',
                                         Headers, exml:to_iolist(Body),
                                         infinity, []),
            Self ! {http_reply, Ref, Reply, Transport}
    end,
    NewRequests = [{Ref, proc_lib:spawn_link(AsyncReq)} | Requests],
    S#state{rid = S#state.rid+1, requests = NewRequests}.

send0(Transport, Elem, State) ->
    send(Transport, wrap_elem(Elem, State), State).

handle_data(<<>>, State) ->
    State;
handle_data(Data, #state{} = State) ->
    {ok, Body} = exml:parse(Data),
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
            forward_to_owner(Stanzas, NewState),
            NewState;
        false ->
            store_reply(Body, NewState)
    end.

forward_to_owner(Stanzas, #state{owner = Owner} = S) ->
    lists:foreach(fun(Stanza) ->
        Owner ! {stanza, transport(S), Stanza}
    end, Stanzas),
    case lists:keyfind(xmlstreamend, 1, Stanzas) of
        false ->
            ok;
        _ ->
            gen_server:cast(self(), stop)
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

transport(#state{url = Url}) ->
    #transport{module = ?MODULE,
               socket = Url,
               ssl = false,
               compress = false,
               rcv_pid = self()}.

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
    catch begin
        case proplists:get_value(<<"type">>, Attrs) of
            <<"terminate">> ->
                throw(streamend);
            _ -> normal
        end,
        case proplists:get_value(<<"xmpp:version">>, Attrs) of
            undefined -> normal;
            Version -> throw({streamstart, Version})
        end
    end.


