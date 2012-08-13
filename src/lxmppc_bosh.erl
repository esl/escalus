%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module abstracting TCP connection to XMPP server
%%% @end
%%%===================================================================

-module(lxmppc_bosh).
-behaviour(gen_server).

-include_lib("exml/include/exml_stream.hrl").
-include("lxmppc.hrl").

%% API exports
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

-define(WAIT_FOR_SOCKET_CLOSE_TIMEOUT, 200).
-define(SERVER, ?MODULE).

-record(state, {owner, url, parser, sid = nil, rid = nil}).

%%%===================================================================
%%% API
%%%===================================================================

-spec connect({binary(), integer()}) -> {ok, #transport{}}.
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

upgrade_to_tls(#transport{socket = Socket, rcv_pid = Pid} = Conn, Props) ->
    not_supported.
use_zlib(#transport{rcv_pid = Pid} = Conn, Props) ->
    not_supported.
get_transport(#transport{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_transport).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Args, Owner]) ->
    Host = proplists:get_value(host, Args, <<"localhost">>),
    Port = proplists:get_value(port, Args, 5280),
    Path = proplists:get_value(path, Args, <<"/http-bind">>),
    HostStr = binary_to_list(Host),
    {MS, S, MMS} = now(),
    InitRid = MS * 1000000 * 1000000 + S * 1000000 + MMS,
    {ok, Parser} = exml_stream:new_parser(),
    {ok, #state{owner = Owner,
                url = {HostStr, Port, binary_to_list(Path)},
                parser = Parser, rid=InitRid}}.

handle_call(get_transport, _From, State) ->
    {reply, transport(State), State};
handle_call(stop, _From, #state{url = Socket} = State) ->
    StreamEnd = lxmppc_stanza:stream_end(),
    send0(transport(State), exml:to_iolist(StreamEnd), State),
    {stop, normal, ok, State}.

handle_cast({send, Transport, Elem}, #state{owner=Owner, rid=Rid} = State) when
        is_record(Elem, xmlstreamstart) ->
        InitBody = send0(Transport, Elem, State),
        NewState = State#state{rid=Rid+1,
                               sid = exml_query:attr(InitBody, <<"sid">>)},
        %%fake stanza with features, clients waits for it
        {noreply, NewState};
handle_cast({send, Transport, Elem}, State) ->
    send0(Transport, Elem, State),
    {noreply, State#state{rid=State#state.rid+1}};
handle_cast(reset_parser, #state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {noreply, State#state{parser = NewParser}}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, #state{parser = Parser}) ->
    exml_stream:free_parser(Parser).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Helpers
%%%===================================================================
send0(#transport{socket = {Host, Port, Path}} = Transport, Elem0, #state{parser = Parser} = State) ->
    Headers = [{"Content-Type", "text/xml; charset-utf-8"}],
    Elem = wrap_elem(Elem0, State),
    {ok, {{StatusCode, Reason}, Hdrs, RespBody}} =
        lhttpc:request(Host, Port, false, Path, 'POST',
            Headers, exml:to_iolist(Elem), infinity, []),
    handle_data(RespBody, State).

handle_data(Data, #state{owner = Owner,
                                 parser = Parser} = State) ->
    {ok, Body} = exml:parse(Data),
    Stanzas = unwrap_elem(Body),
    lists:foreach(fun(Stanza) ->
        Owner ! {stanza, transport(State), Stanza}
    end, Stanzas),
    Body.

transport(#state{url = Url}) ->
    #transport{module = ?MODULE,
               socket = Url,
               ssl = false,
               compress = false,
               rcv_pid = self()}.

wrap_elem(#xmlstreamstart{attrs=Attrs}, #state{rid=Rid}) ->
    Version = proplists:get_value(<<"version">>, Attrs, <<"1.0">>),
    Lang = proplists:get_value(<<"xml:lang">>, Attrs, <<"en">>),
    To = proplists:get_value(<<"to">>, Attrs, <<"localhost">>),
    #xmlelement{name = <<"body">>, attrs=common_attrs(Rid) ++ [
            {<<"content">>, <<"text/xml; charset=utf-8">>},
            {<<"hold">>, <<"1">>},
            {<<"wait">>, <<"60">>},
            {<<"xml:lang">>, Lang},
            {<<"to">>, To}
            ]};
wrap_elem(["</", <<"stream:stream">>, ">"], #state{sid=Sid, rid=Rid}) ->
    #xmlelement{name = <<"body">>, 
                attrs = common_attrs(Rid, Sid) ++ [{<<"type">>, <<"terminate">>}],
                body = [#xmlelement{name = <<"presence">>,
                                   attrs = [
                                        {<<"type">>, <<"unavailable">>},
                                        {<<"xmlns">>, <<"jabber:client">>}
                    ]}]};
wrap_elem(Element, #state{sid = Sid, rid=Rid}) ->
    #xmlelement{name = <<"body">>, 
            attrs = common_attrs(Rid, Sid),
            body = [Element]}.

common_attrs(Rid) ->
    [{<<"rid">>, pack_rid(Rid)},
     {<<"xmlns">>, <<"http://jabber.org/protocol/httpbind">>}].
common_attrs(Rid, Sid) ->
    common_attrs(Rid) ++ [{<<"sid">>, Sid}].

pack_rid(Rid) ->
    list_to_binary(integer_to_list(Rid)).

unwrap_elem(#xmlelement{name = <<"body">>, body = [], attrs=Attrs}) ->
    case proplists:get_value(<<"from">>, Attrs) of
        undefined -> [];
        Server ->
            StreamStart = #xmlstreamstart{name = <<"stream:stream">>, attrs=[
                        {<<"from">>, Server},
                        {<<"version">>, <<"1.0">>},
                        {<<"xml:lang">>, <<"en">>},
                        {<<"xmlns">>, <<"jabber:client">>},
                        {<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>}]},
            StreamFeatures = #xmlelement{
                    name = <<"stream:features">>,
                    attrs= [], body = []},
            [StreamStart, StreamFeatures]
    end;
unwrap_elem(#xmlelement{name = <<"body">>, body = Body}) ->
    Body.

