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
         reset_parser/1,
         get_sm_h/1,
         set_sm_h/2,
         use_zlib/1,
         upgrade_to_tls/2,
         set_filter_predicate/2,
         stop/1,
         kill/1]).
%% Connection stream start and end callbacks
-export([stream_start_req/1,
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
                event_client, sm_state, filter_pred, stream_ref, sent_stanzas = []}).
-type state() :: #state{}.

-type sm_state() :: {boolean(), non_neg_integer(), 'active'|'inactive'}.

-type opts() :: #{
      host               => string(),
      port               => pos_integer(),
      wspath             => string(),
      wslegacy           => boolean(),
      event_client       => undefined | escalus_event:event_client(),
      ssl                => boolean(),
      ssl_opts           => [ssl:ssl_option()],
      ws_upgrade_timeout => pos_integer(),
      stream_management  => boolean(),
      manual_ack         => boolean()
}.

%%%===================================================================
%%% API
%%%===================================================================

-spec connect([proplists:property()]) -> pid().
connect(Opts0) ->
    Opts1 = opts_to_map(Opts0),
    {ok, Pid} = gen_server:start_link(?MODULE, [Opts1, self()], []),
    Pid.

-spec send(pid(), exml:element()) -> ok.
send(Pid, Elem) ->
    gen_server:cast(Pid, {send, Elem}).

-spec is_connected(pid()) -> boolean().
is_connected(Pid) ->
    erlang:is_process_alive(Pid).

-spec get_sm_h(pid()) -> non_neg_integer().
get_sm_h(Pid) ->
    gen_server:call(Pid, get_sm_h).

-spec set_sm_h(pid(), non_neg_integer()) -> {ok, non_neg_integer()}.
set_sm_h(Pid, H) ->
    gen_server:call(Pid, {set_sm_h, H}).

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
%%% Default options
%%%===================================================================

default_options() ->
    #{host               => "localhost",
      port               => 5280,
      wspath             => "/ws-xmpp",
      wslegacy           => false,
      event_client       => undefined,
      ssl                => false,
      ssl_opts           => [],
      ws_upgrade_timeout => 5000,
      stream_management  => false,
      manual_ack         => false}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(list()) -> {ok, state()}.
init([Opts, Owner]) ->
    Opts1 = overwrite_default_opts(Opts, default_options()),
    #{wspath := Resource,
      wslegacy := LegacyWS,
      event_client := EventClient,
      ws_upgrade_timeout := Timeout} = Opts1,
    SM = get_stream_management_opt(Opts1),
    Resource1 = maybe_binary_to_list(Resource),

    ConnPid = do_connect(Opts1),

    %% Disable http2 in protocols
    WSUpgradeHeaders = [{<<"sec-websocket-protocol">>, <<"xmpp">>}],
    StreamRef = gun:ws_upgrade(ConnPid, Resource1, WSUpgradeHeaders,
                               #{protocols => [{<<"xmpp">>, gun_ws_h}]}),
    wait_for_ws_upgrade(ConnPid, StreamRef, Timeout),
    ParserOpts = case LegacyWS of
                     true -> [];
                     _ -> [{infinite_stream, true}, {autoreset, true}]
                 end,
    {ok, Parser} = exml_stream:new_parser(ParserOpts),
    {ok, #state{owner = Owner,
                socket = ConnPid,
                parser = Parser,
                legacy_ws = LegacyWS,
                sm_state = SM,
                event_client = EventClient,
                stream_ref = StreamRef}}.

wait_for_ws_upgrade(ConnPid, StreamRef, Timeout) ->
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _} ->
            ok;
        {gun_response, ConnPid, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, ConnPid, _StreamRef, Reason} ->
            exit({ws_upgrade_failed, Reason})
    after %% More clauses here as needed.
          Timeout ->
            exit(ws_upgrade_timeout)
    end.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {stop, normal, ok, state()}.
handle_call(get_sm_h, _From, #state{sm_state = {_, H, _}} = State) ->
    {reply, H, State};
handle_call({set_sm_h, H}, _From, #state{sm_state = {A, _OldH, S}} = State) ->
    NewState = State#state{sm_state={A, H, S}},
    {reply, {ok, H}, NewState};
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
handle_call(kill_connection, _, #state{socket = ConnPid} = State) ->
    gun:close(ConnPid),
    {stop, normal, ok, State};
handle_call(stop, _From, #state{socket = ConnPid, stream_ref = StreamRef} = State) ->
    close_compression_streams(State#state.compress),
    gun:ws_send(ConnPid, StreamRef, close),
    {stop, normal, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({send, Elem}, State) ->
    Data = case State#state.compress of
               {zlib, {_, Zout}} -> zlib:deflate(Zout, exml:to_iolist(Elem), sync);
               false -> exml:to_iolist(Elem)
           end,
    gun:ws_send(State#state.socket, State#state.stream_ref, {text, Data}),
    {noreply, State};
handle_cast(reset_parser, #state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {noreply, State#state{parser = NewParser}}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info(tcp_closed, State) ->
    {stop, normal, State};
handle_info({error, Reason}, State) ->
    {stop, Reason, State};
handle_info({gun_ws, ConnPid, _StreamRef, {text, Data}}, #state{socket = ConnPid} = State) ->
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

-spec get_stream_management_opt(opts()) -> sm_state().
get_stream_management_opt(#{stream_management := false}) ->
    {false, 0, inactive};
get_stream_management_opt(#{manual_ack := true}) ->
    {false, 0, inactive};
get_stream_management_opt(#{stream_management := true, manual_ack := false}) ->
    {true, 0, inactive}.

overwrite_default_opts(GivenOpts, DefaultOpts) ->
    maps:merge(DefaultOpts, GivenOpts).

do_connect(#{ssl := true, ssl_opts := SSLOpts} = Opts) ->
    TransportOpts = #{transport => tls, protocols => [http],
                      tls_opts => SSLOpts},
    do_connect(Opts, TransportOpts);
do_connect(Opts) ->
    do_connect(Opts, #{transport => tcp, protocols => [http]}).

do_connect(#{host := Host, port := Port}, TransportOpts) ->
    Host1 = maybe_binary_to_list(Host),
    {ok, ConnPid} = gun:open(Host1, Port, TransportOpts),
    {ok, http} = gun:await_up(ConnPid),
    ConnPid.

handle_data(Data, State = #state{parser = Parser,
                                 compress = Compress}) ->
    Timestamp = os:system_time(microsecond),
    {ok, NewParser, Stanzas} =
        case Compress of
            false ->
                exml_stream:parse(Parser, Data);
            {zlib, {Zin,_}} ->
                Decompressed = iolist_to_binary(zlib:inflate(Zin, Data)),
                exml_stream:parse(Parser, Decompressed)
        end,
    FwdState = State#state{parser = NewParser, sent_stanzas = []},
    NewState = escalus_connection:maybe_forward_to_owner(FwdState#state.filter_pred,
                                                         FwdState,
                                                         Stanzas,
                                                         fun forward_to_owner/3, Timestamp),
    case lists:filter(fun(Stanza) -> is_stream_end(Stanza, State) end, Stanzas) of
        [] -> {noreply, NewState};
        _ -> {stop, normal, NewState}
    end.

-spec is_stream_end(exml_stream:element(), state()) -> boolean().
is_stream_end(#xmlstreamend{}, #state{legacy_ws = true}) -> true;
is_stream_end(#xmlel{name = <<"close">>}, #state{legacy_ws = false}) -> true;
is_stream_end(_, _) -> false.

forward_to_owner(Stanzas0, #state{owner = Owner,
                                  sm_state = SM0,
                                  event_client = EventClient} = State, Timestamp) ->
    {SM1, AckRequests, StanzasNoRs} = escalus_connection:separate_ack_requests(SM0, Stanzas0),
    reply_to_ack_requests(SM1, AckRequests, State),

    lists:foreach(fun(Stanza) ->
        escalus_event:incoming_stanza(EventClient, Stanza),
        Owner ! escalus_connection:stanza_msg(Stanza, #{recv_timestamp => Timestamp})
    end, StanzasNoRs),

    State#state{sm_state = SM1, sent_stanzas = StanzasNoRs}.

reply_to_ack_requests({false, H, A}, _, _) ->
    {false, H, A};
reply_to_ack_requests({true, H, inactive}, _, _) ->
    {true, H, inactive};
reply_to_ack_requests({true, H0, active}, Acks, State) ->
    {true,
     lists:foldl(fun({Ack, H}, _) ->
        Ack1 = exml:to_iolist(Ack),
        gun:ws_send(State#state.socket, State#state.stream_ref, {text, Ack1}),
        H
     end, H0, Acks),
     active}.

common_terminate(_Reason, #state{parser = Parser}) ->
    exml_stream:free_parser(Parser).

-spec maybe_binary_to_list(binary() | string()) -> string().
maybe_binary_to_list(B) when is_binary(B) -> binary_to_list(B);
maybe_binary_to_list(S) when is_list(S) -> S.

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

-spec opts_to_map(proplists:proplist() | opts()) -> opts().
opts_to_map(Opts) when is_map(Opts) -> Opts;
opts_to_map(Opts) when is_list(Opts) -> maps:from_list(Opts).
