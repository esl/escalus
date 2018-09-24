%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Module abstracting Websockets over TCP connection to XMPP server
%%% @end
%%%===================================================================

-module(escalus_ws).
-behaviour(gen_server).
-behaviour(escalus_connection).

-include_lib("exml/include/exml_stream.hrl").
-include("escalus.hrl").

%% API exports
-export([connect/1,
         send/2,
         is_connected/1,
         upgrade_to_tls/2,
         use_zlib/1,
         reset_parser/1,
         set_filter_predicate/2,
         stop/1,
         kill/1,
         stream_start_req/1,
         stream_end_req/1,
         assert_stream_start/2,
         assert_stream_end/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(WAIT_FOR_SOCKET_CLOSE_TIMEOUT, 200).
-define(HANDSHAKE_TIMEOUT, 3000).
-define(SERVER, ?MODULE).

-record(state, {owner, socket, parser, legacy_ws, compress = false,
                event_client, filter_pred}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec connect([proplists:property()]) -> pid().
connect(Args) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Args, self()], []),
    Pid.

-spec send(pid(), exml:element()) -> ok.
send(Pid, Elem) ->
    gen_server:cast(Pid, {send, Elem}).

-spec is_connected(pid()) -> boolean().
is_connected(Pid) ->
    erlang:is_process_alive(Pid).

-spec reset_parser(pid()) -> ok.
reset_parser(Pid) ->
    gen_server:cast(Pid, reset_parser).

-spec stop(pid()) -> ok | already_stopped.
stop(Pid) ->
    try
        gen_server:call(Pid, stop)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            already_stopped;
        exit:{normal, {gen_server, call, _}} ->
            already_stopped
    end.

-spec kill(pid()) -> ok | already_stopped.
kill(Pid) ->
    %% Use `kill_connection` to avoid confusion with exit reason `kill`.
    try
        gen_server:call(Pid, kill_connection)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            already_stopped;
        exit:{normal, {gen_server, call, _}} ->
            already_stopped
    end.

-spec set_filter_predicate(pid(), escalus_connection:filter_pred()) -> ok.
set_filter_predicate(Pid, Pred) ->
    gen_server:call(Pid, {set_filter_pred, Pred}).

-spec upgrade_to_tls(_, _) -> no_return().
upgrade_to_tls(_, _) ->
    throw(starttls_not_supported).

-spec use_zlib(pid()) -> ok.
use_zlib(Pid) ->
    gen_server:call(Pid, use_zlib).

-spec stream_start_req(escalus_users:user_spec()) -> exml_stream:element().
stream_start_req(Props) ->
    {server, Server} = lists:keyfind(server, 1, Props),
    case proplists:get_value(wslegacy, Props, false) of
        true ->
            NS = proplists:get_value(stream_ns, Props, <<"jabber:client">>),
            escalus_stanza:stream_start(Server, NS);
        false ->
            escalus_stanza:ws_open(Server)
    end.

-spec stream_end_req(_) -> exml_stream:element().
stream_end_req(Props) ->
    case proplists:get_value(wslegacy, Props, false) of
        true -> escalus_stanza:stream_end();
        false -> escalus_stanza:ws_close()
    end.

-spec assert_stream_start(exml_stream:element(), _) -> exml_stream:element().
assert_stream_start(StreamStartRep, Props) ->
    case {StreamStartRep, proplists:get_value(wslegacy, Props, false)} of
        {#xmlel{name = <<"open">>}, false} ->
            StreamStartRep;
        {#xmlel{name = <<"open">>}, true} ->
            error("<open/> with legacy WebSocket",
                  [StreamStartRep]);
        {#xmlstreamstart{}, false} ->
            error("<stream:stream> with non-legacy WebSocket",
                  [StreamStartRep]);
        {#xmlstreamstart{}, _} ->
            StreamStartRep;
        _ ->
            error("Not a valid stream start", [StreamStartRep])
    end.

-spec assert_stream_end(exml_stream:element(), _) -> exml_stream:element().
assert_stream_end(StreamEndRep, Props) ->
    case {StreamEndRep, proplists:get_value(wslegacy, Props, false)} of
        {#xmlel{name = <<"close">>}, false} ->
            StreamEndRep;
        {#xmlel{name = <<"close">>}, true} ->
            error("<close/> with legacy WebSocket",
                  [StreamEndRep]);
        {#xmlstreamend{}, false} ->
            error("<stream:stream> with non-legacy WebSocket",
                  [StreamEndRep]);
        {#xmlstreamend{}, _} ->
            StreamEndRep;
        _ ->
            error("Not a valid stream end", [StreamEndRep])
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% TODO: refactor all opt defaults taken from Args into a default_opts function,
%%       so that we know what options the module actually expects
-spec init(list()) -> {ok, state()}.
init([Args, Owner]) ->
    Host = get_host(Args, "localhost"),
    Port = get_port(Args, 5280),
    Resource = get_resource(Args, "/ws-xmpp"),
    LegacyWS = get_legacy_ws(Args, false),
    EventClient = proplists:get_value(event_client, Args),
    SSL = proplists:get_value(ssl, Args, false),
    %% Disable http2 in protocols
    WSOptions = case SSL of
                    true ->
                        #{transport => ssl, protocols => [http]};
                    _ ->
                        #{transport => tcp, protocols => [http]}
                end,
    {ok, ConnPid} = gun:open(Host, Port, WSOptions),
    {ok, http} = gun:await_up(ConnPid),
    WSUpgradeHeaders = [{<<"sec-websocket-protocol">>, <<"xmpp">>}],
    gun:ws_upgrade(ConnPid, Resource, WSUpgradeHeaders,
                   #{protocols => [{<<"xmpp">>, gun_ws_handler}]}),
    wait_for_ws_upgrade(ConnPid),
    ParserOpts = case LegacyWS of
                     true -> [];
                     _ -> [{infinite_stream, true}, {autoreset, true}]
                 end,
    {ok, Parser} = exml_stream:new_parser(ParserOpts),
    {ok, #state{owner = Owner,
                socket = ConnPid,
                parser = Parser,
                legacy_ws = LegacyWS,
                event_client = EventClient}}.

wait_for_ws_upgrade(ConnPid) ->
    receive
        {gun_ws_upgrade, ConnPid, ok, _Headers} ->
            ok;
        {gun_response, ConnPid, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, ConnPid, _StreamRef, Reason} ->
            exit({ws_upgrade_failed, Reason})
    after %% More clauses here as needed.
          1000 ->
            exit(ws_upgrade_timeout)
    end.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {stop, normal, ok, state()}.
handle_call(use_zlib, _, #state{parser = Parser} = State) ->
    Zin = zlib:open(),
    Zout = zlib:open(),
    ok = zlib:inflateInit(Zin),
    ok = zlib:deflateInit(Zout),
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {reply, ok, State#state{parser = NewParser,
                            compress = {zlib, {Zin, Zout}}}};
handle_call({set_filter_pred, Pred}, _From, State) ->
    {reply, ok, State#state{filter_pred = Pred}};
handle_call(kill_connection, _, S) ->
    {stop, normal, ok, S};
handle_call(stop, _From, #state{socket = ConnPid} = State) ->
    close_compression_streams(State#state.compress),
    gun:ws_send(ConnPid, close),
    {stop, normal, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({send, Elem}, State) ->
    Data = case State#state.compress of
               {zlib, {_, Zout}} -> zlib:deflate(Zout, exml:to_iolist(Elem), sync);
               false -> exml:to_iolist(Elem)
           end,
    gun:ws_send(State#state.socket, {text, Data}),
    {noreply, State};
handle_cast(reset_parser, #state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {noreply, State#state{parser = NewParser}}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info(tcp_closed, State) ->
    {stop, normal, State};
handle_info({error, Reason}, State) ->
    {stop, Reason, State};
handle_info({gun_ws, ConnPid, {text, Data}}, #state{socket = ConnPid} = State) ->
    handle_data(Data, State);
handle_info(_, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> term().
terminate(Reason, State) ->
    common_terminate(Reason, State).

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Helpers
%%%===================================================================

handle_data(Data, State = #state{parser = Parser,
                                 compress = Compress}) ->
    Timestamp = os:system_time(micro_seconds),
    {ok, NewParser, Stanzas} =
        case Compress of
            false ->
                exml_stream:parse(Parser, Data);
            {zlib, {Zin,_}} ->
                Decompressed = iolist_to_binary(zlib:inflate(Zin, Data)),
                exml_stream:parse(Parser, Decompressed)
        end,
    NewState = State#state{parser = NewParser},
    escalus_connection:maybe_forward_to_owner(NewState#state.filter_pred,
                                              NewState,
                                              Stanzas,
                                              fun forward_to_owner/3, Timestamp),
    case lists:filter(fun is_stream_end/1, Stanzas) of
        [] -> {noreply, NewState};
        _ -> {stop, normal, NewState}
    end.

-spec is_stream_end(exml_stream:element()) -> boolean().
is_stream_end(#xmlstreamend{}) -> true;
is_stream_end(_) -> false.

forward_to_owner(Stanzas, #state{owner = Owner,
                                 event_client = EventClient}, Timestamp) ->
    lists:foreach(fun(Stanza) ->
                          escalus_event:incoming_stanza(EventClient, Stanza),
                          Owner ! escalus_connection:stanza_msg(Stanza,
                                                                #{recv_timestamp => Timestamp})
                  end, Stanzas).

common_terminate(_Reason, #state{parser = Parser}) ->
    exml_stream:free_parser(Parser).

-spec get_port(list(), inet:port_number()) -> inet:port_number().
get_port(Args, Default) ->
    get_option(port, Args, Default).

-spec get_host(list(), string()) -> string().
get_host(Args, Default) ->
    maybe_binary_to_list(get_option(host, Args, Default)).

-spec get_resource(list(), string()) -> string().
get_resource(Args, Default) ->
    maybe_binary_to_list(get_option(wspath, Args, Default)).

-spec get_legacy_ws(list(), boolean()) -> boolean().
get_legacy_ws(Args, Default) ->
    get_option(wslegacy, Args, Default).

-spec maybe_binary_to_list(binary() | string()) -> string().
maybe_binary_to_list(B) when is_binary(B) -> binary_to_list(B);
maybe_binary_to_list(S) when is_list(S) -> S.

-spec get_option(any(), list(), any()) -> any().
get_option(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        false -> Default;
        {Key, Value} -> Value
    end.

close_compression_streams(false) ->
    ok;
close_compression_streams({zlib, {Zin, Zout}}) ->
    try
        zlib:deflate(Zout, <<>>, finish),
        ok = zlib:inflateEnd(Zin),
        ok = zlib:deflateEnd(Zout)
    catch
        error:data_error -> ok
    after
        ok = zlib:close(Zin),
        ok = zlib:close(Zout)
    end.


