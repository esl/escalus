%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module abstracting TCP connection to XMPP server
%%% @end
%%%===================================================================

-module(escalus_tcp).
-behaviour(gen_server).
-behaviour(escalus_connection).

-include_lib("exml/include/exml_stream.hrl").

%% Escalus transport callbacks
-export([connect/1,
         send/2,
         is_connected/1,
         reset_parser/1,
         use_zlib/1,
         upgrade_to_tls/2,
         set_filter_predicate/2,
         stop/1,
         kill/1,
         get_sm_h/1,
         set_sm_h/2,
         is_using_compression/1,
         is_using_ssl/1,
         get_tls_last_message/1
        ]).
%% Connection stream start and end callbacks
-export([stream_start_req/1,
         stream_end_req/1,
         assert_stream_start/2,
         assert_stream_end/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Low level API
-export([get_active/1,
         set_active/2]).

-ifdef(EUNIT_TEST).
-compile(export_all).
-endif.

%% Stream management automation
%%               :: {Auto-ack?,                H,         counting Hs?}.
-type sm_state() :: {boolean(), non_neg_integer(), 'active'|'inactive'}.
-export_type([sm_state/0]).

-define(WAIT_FOR_SOCKET_CLOSE_TIMEOUT, 1000).
-include("escalus_tcp.hrl").

-type state() :: #state{}.
-type opts() :: #{
        host              => binary() | inet:ip_address() | inet:hostname(),
        port              => pos_integer(),
        ssl               => boolean(),
        stream_management => boolean(),
        manual_ack        => boolean(),
        iface             => inet:ip_address(),
        on_reply          => fun(),
        on_request        => fun(),
        on_connect        => fun(),
        event_client      => undefined | escalus_event:event_client(),
        socket_opts       => [gen_tcp:connect_option()],
        ssl_opts          => [ssl:tls_option()],
        parser_opts       => [exml_stream:parser_opt()],
        hibernate_after   => timeout()
}.

%%%===================================================================
%%% API
%%%===================================================================

-spec connect([proplists:property()] | opts()) -> pid().
connect(Opts0) ->
    Opts1 = opts_to_map(Opts0),
    Opts2 = overwrite_default_opts(Opts1, default_options()),
    GenOpts = maps:to_list(maps:with([hibernate_after], Opts1)),
    {ok, Pid} = gen_server:start_link(?MODULE, {Opts2, self()}, GenOpts),
    Pid.

-spec send(pid(), exml_stream:element() | [exml_stream:element()] | binary()) -> ok.
send(Pid, ElemOrData) ->
    gen_server:cast(Pid, {send, ElemOrData}).

-spec is_connected(pid()) -> boolean().
is_connected(Pid) ->
    erlang:is_process_alive(Pid).

-spec reset_parser(pid()) -> ok.
reset_parser(Pid) ->
    gen_server:cast(Pid, reset_parser).

-spec get_sm_h(pid()) -> non_neg_integer().
get_sm_h(Pid) ->
    gen_server:call(Pid, get_sm_h).

-spec set_sm_h(pid(), non_neg_integer()) -> {ok, non_neg_integer()}.
set_sm_h(Pid, H) ->
    gen_server:call(Pid, {set_sm_h, H}).

-spec is_using_compression(pid()) -> boolean().
is_using_compression(Pid) ->
    gen_server:call(Pid, get_compress) =/= false.

-spec is_using_ssl(pid()) -> boolean().
is_using_ssl(Pid) ->
    gen_server:call(Pid, get_ssl).

-spec set_filter_predicate(pid(), escalus_connection:filter_pred()) -> ok.
set_filter_predicate(Pid, Pred) ->
    gen_server:call(Pid, {set_filter_pred, Pred}).

-spec get_tls_last_message(pid()) -> {ok, binary()} | {error, undefined_tls_message}.
get_tls_last_message(Pid) ->
    gen_server:call(Pid, get_tls_last_message).

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

-spec upgrade_to_tls(pid(), [ssl:tls_option()]) -> ok.
upgrade_to_tls(Pid, SSLOpts) ->
    case gen_server:call(Pid, {upgrade_to_tls, SSLOpts}) of
        {error, Error} ->
            error(Error);
        _ ->
            ok
    end.

-spec use_zlib(pid()) -> ok.
use_zlib(Pid) ->
    gen_server:call(Pid, use_zlib).

-spec stream_start_req(escalus_users:user_spec()) -> exml_stream:element().
stream_start_req(Props) ->
    {server, Server} = lists:keyfind(server, 1, Props),
    NS = proplists:get_value(stream_ns, Props, <<"jabber:client">>),
    escalus_stanza:stream_start(Server, NS).

-spec stream_end_req(_) -> exml_stream:element().
stream_end_req(_) ->
    escalus_stanza:stream_end().

-spec assert_stream_start(exml_stream:element(), _) -> exml_stream:element().
assert_stream_start(Rep = #xmlstreamstart{}, _) -> Rep;
assert_stream_start(Rep, _) -> error("Not a valid stream start", [Rep]).

-spec assert_stream_end(exml_stream:element(), _) -> exml_stream:element().
assert_stream_end(Rep = #xmlstreamend{}, _) -> Rep;
assert_stream_end(Rep, _) -> error("Not a valid stream end", [Rep]).


%%%===================================================================
%%% Low level API
%%%===================================================================

-spec get_active(pid()) -> boolean().
get_active(Pid) ->
    gen_server:call(Pid, get_active).

-spec set_active(pid(), boolean() | once) -> ok.
set_active(Pid, Active) ->
    gen_server:call(Pid, {set_active, Active}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init({opts(), pid()}) -> {ok, state()}.
init({Opts, Owner}) ->
    #{ssl          := IsSSLConnection,
      on_reply     := OnReplyFun,
      on_request   := OnRequestFun,
      parser_opts  := ParserOpts,
      event_client := EventClient} = Opts,
    SM = get_stream_management_opt(Opts),

    {ok, Socket} = do_connect(Opts),
    {ok, Parser} = exml_stream:new_parser(ParserOpts),
    {ok, #state{owner = Owner,
                socket = Socket,
                parser = Parser,
                ssl = IsSSLConnection,
                sm_state = SM,
                event_client = EventClient,
                on_reply = OnReplyFun,
                on_request = OnRequestFun}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {stop, normal, ok, state()}.
handle_call(get_sm_h, _From, #state{sm_state = {_, H, _}} = State) ->
    {reply, H, State};
handle_call({set_sm_h, H}, _From, #state{sm_state = {A, _OldH, S}} = State) ->
    NewState = State#state{sm_state={A, H, S}},
    {reply, {ok, H}, NewState};
handle_call({upgrade_to_tls, SSLOpts}, _From, #state{socket = Socket} = State) ->
    case ssl:connect(Socket, SSLOpts) of
        {ok, TlsSocket} ->
            {ok, Parser} = exml_stream:new_parser(),
            {reply, TlsSocket, State#state{socket = TlsSocket, parser = Parser, ssl = true}};
        {error, _} = E ->
            {reply, E, State}
    end;
handle_call(use_zlib, _, #state{parser = Parser} = State) ->
    Zin = zlib:open(),
    Zout = zlib:open(),
    ok = zlib:inflateInit(Zin),
    ok = zlib:deflateInit(Zout),
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {reply, ok, State#state{parser = NewParser,
                            compress = {zlib, {Zin, Zout}}}};
handle_call(get_active, _From, #state{active = Active} = State) ->
    {reply, Active, State};
handle_call(get_compress, _From, #state{compress = Compress} = State) ->
    {reply, Compress, State};
handle_call(get_ssl, _From, #state{ssl = false} = State) ->
    {reply, false, State};
handle_call(get_ssl, _From, #state{ssl = _} = State) ->
    {reply, true, State};
handle_call({set_active, Active}, _From, State) ->
    {reply, ok, set_active_opt(State, Active)};
handle_call({set_filter_pred, Pred}, _From, State) ->
    {reply, ok, State#state{filter_pred = Pred}};
handle_call(get_tls_last_message, _From, #state{} = S) ->
    {reply, {error, undefined_tls_message}, S};
handle_call(kill_connection, _, #state{socket = Socket, ssl = SSL} = S) ->
    case SSL of
        true -> ssl:close(Socket);
        false -> gen_tcp:close(Socket)
    end,
    close_compression_streams(S#state.compress),
    {stop, normal, ok, S};
handle_call(stop, _From, S) ->
    close_compression_streams(S#state.compress),
    wait_until_closed(S#state.socket),
    {stop, normal, ok, S}.

-spec handle_cast({send, exml_stream:element() | [exml_stream:element()] | binary()}, state()) ->
    {noreply, state()} | {stop, term(), state()}.
handle_cast({send, Data}, #state{ on_request = OnRequestFun } = State)  when is_binary(Data) ->
    OnRequestFun(maybe_compress_and_send(Data, State)),
    {noreply, State};
handle_cast({send, StreamLevelElement}, #state{ on_request = OnRequestFun } = State) ->
    OnRequestFun(maybe_compress_and_send(exml:to_iolist(StreamLevelElement), State)),
    {noreply, State};
handle_cast(reset_parser, #state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {noreply, State#state{parser = NewParser}};
handle_cast(stop, State) ->
    {stop, normal, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info({tcp, Socket, Data}, #state{socket = Socket, ssl = false} = State) ->
    NewState = handle_data(Socket, Data, State),
    {noreply, NewState};
handle_info({ssl, Socket, Data}, #state{socket = Socket, ssl = true} = State) ->
    NewState = handle_data(Socket, Data, State),
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, #state{} = State) ->
    {stop, normal, State};
handle_info({ssl_closed, _Socket}, #state{} = State) ->
    {stop, normal, State};
handle_info({tcp_error, _Socket, Reason}, #state{} = State) ->
    {stop, {error, Reason}, State};
handle_info({ssl_error, _Socket, Reason}, #state{} = State) ->
    {stop, {error, Reason}, State};
handle_info(_, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> term().
terminate(_Reason, #state{socket = Socket, ssl = true, parser = Parser}) ->
    exml_stream:free_parser(Parser),
    ssl:close(Socket);
terminate(_Reason, #state{socket = Socket, parser = Parser}) ->
    exml_stream:free_parser(Parser),
    gen_tcp:close(Socket).

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Default options
%%%===================================================================

-spec default_options() -> opts().
default_options() ->
    #{host              => <<"localhost">>,
      port              => 5222,
      ssl               => false,
      stream_management => false,
      manual_ack        => false,
      on_reply          => fun(_) -> ok end,
      on_request        => fun(_) -> ok end,
      on_connect        => fun(_) -> ok end,
      event_client      => undefined,
      socket_opts       => default_socket_options(),
      ssl_opts          => [{verify, verify_none}],
      parser_opts       => [],
      hibernate_after   => 500}.

-spec default_socket_options() -> [gen_tcp:connect_option()].
default_socket_options() ->
    [binary,
     {active, once},
     {reuseaddr, true},
     {nodelay, true}
    ].


%%%===================================================================
%%% Helpers
%%%===================================================================
set_active_opt(
  #state{ssl = SSL, socket = Soc} = State, Act) when is_boolean(Act) ->
    set_active_opt(SSL, Soc, Act),
    State#state{active = Act};
set_active_opt(
  #state{ssl = SSL, socket = Soc, active = Act} = State, current_opt) ->
    set_active_opt(SSL, Soc, Act),
    State;
set_active_opt(#state{ssl = SSL, socket = Soc} = State, once) ->
    set_active_opt(SSL, Soc, true),
    State#state{active = false};
set_active_opt(#state{ssl = SSL, socket = Soc} = State, at_least_once) ->
    set_active_opt(SSL, Soc, true),
    State.

set_active_opt(true, Socket, true) ->
    ssl:setopts(Socket, [{active, once}]);
set_active_opt(false, Socket, true) ->
    inet:setopts(Socket, [{active, once}]);
set_active_opt(_, _, _) ->
    ok.

handle_data(Socket, Data, #state{parser      = Parser,
                                 socket      = Socket,
                                 compress    = Compress,
                                 on_reply    = OnReplyFun,
                                 filter_pred = Filter} = State) ->
    Timestamp = os:system_time(microsecond),
    set_active_opt(State, current_opt),
    OnReplyFun({erlang:byte_size(Data)}),
    {ok, NewParser, Stanzas} =
    case Compress of
        false ->
            exml_stream:parse(Parser, Data);
        {zlib, {Zin, _}} ->
            Decompressed = iolist_to_binary(zlib:inflate(Zin, Data)),
            exml_stream:parse(Parser, Decompressed)
    end,
    FwdState = State#state{parser = NewParser, sent_stanzas = []},
    NewState = escalus_connection:maybe_forward_to_owner(Filter, FwdState, Stanzas,
                                                         fun forward_to_owner/3, Timestamp),
    %% set active option if nothing is forwarded to owner pid
    case NewState#state.sent_stanzas of
        [] -> set_active_opt(NewState, at_least_once);
        _ -> NewState
    end.

forward_to_owner(Stanzas0, #state{owner = Owner,
                                  sm_state = SM0,
                                  event_client = EventClient} = State, Timestamp) ->
    {SM1, AckRequests, StanzasNoRs} = escalus_connection:separate_ack_requests(SM0, Stanzas0),
    reply_to_ack_requests(SM1, AckRequests, State),

    lists:foreach(fun(Stanza) ->
        escalus_event:incoming_stanza(EventClient, Stanza),
        Owner ! escalus_connection:stanza_msg(Stanza, #{recv_timestamp => Timestamp})
    end, StanzasNoRs),

    case lists:keyfind(xmlstreamend, 1, StanzasNoRs) of
        false -> ok;
        _     -> gen_server:cast(self(), stop)
    end,

    State#state{sm_state = SM1, sent_stanzas = StanzasNoRs}.

reply_to_ack_requests({false, H, A}, _, _) -> {false, H, A};
reply_to_ack_requests({true, H, inactive}, _, _) -> {true, H, inactive};
reply_to_ack_requests({true, H0, active}, Acks, State) ->
    {true,
     % TODO: Maybe compress here?
     lists:foldl(fun({Ack, H}, _) -> raw_send(exml:to_iolist(Ack), State), H end,
                 H0, Acks),
     active}.

maybe_compress_and_send(Data, #state{ compress = {zlib, {_, Zout}} } = State) ->
    raw_send(zlib:deflate(Zout, Data, sync), State);
maybe_compress_and_send(Data, State) ->
    raw_send(Data, State).

raw_send(Data, #state{socket = Socket, ssl = true}) ->
    ssl:send(Socket, Data);
raw_send(Data, #state{socket = Socket}) ->
    gen_tcp:send(Socket, Data).

wait_until_closed(Socket) ->
    receive
        {tcp_closed, Socket} ->
            ok
    after ?WAIT_FOR_SOCKET_CLOSE_TIMEOUT ->
            %% Make warning, but allow process exit without an error.
            %% There are many reasons for this to happen.
            error_logger:warning_msg("tcp_close_timeout ~p~n", [Socket]),
            {error, tcp_close_timeout}
    end.

-spec host_to_inet(tuple() | atom() | list() | binary())
    -> inet:ip_address() | inet:hostname().
host_to_inet({_,_,_,_} = IP4) -> IP4;
host_to_inet({_,_,_,_,_,_,_,_} = IP6) -> IP6;
host_to_inet(Address) when is_list(Address) orelse is_atom(Address) -> Address;
host_to_inet(BAddress) when is_binary(BAddress) -> binary_to_list(BAddress).

iface_to_ip_address({_,_,_,_} = IP4) -> IP4;
iface_to_ip_address({_,_,_,_,_,_,_,_} = IP6) -> IP6.

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

do_connect(#{ssl        := IsSSLConn,
             on_connect := OnConnectFun,
             host       := Host,
             port       := Port,
             ssl_opts   := SSLOpts,
             hibernate_after := HibernateAfter} = Opts) ->
    Address = host_to_inet(Host),
    SocketOpts = get_socket_opts(Opts),
    TimeB = erlang:system_time(microsecond),
    Reply = maybe_ssl_connection(IsSSLConn, Address, Port, SocketOpts, SSLOpts, HibernateAfter),
    TimeA = erlang:system_time(microsecond),
    ConnectionTime = TimeA - TimeB,
    case Reply of
        {ok, Socket} ->
            OnConnectFun({ok, Socket, ConnectionTime});
        {error, _} ->
            OnConnectFun(Reply)
    end,
    Reply.

maybe_ssl_connection(true, Address, Port, SocketOpts, SSLOpts, HibernateAfter) ->
    ssl:connect(Address, Port, SocketOpts ++ SSLOpts ++ [{hibernate_after, HibernateAfter}]);
maybe_ssl_connection(_, Address, Port, SocketOpts, _, _) ->
    gen_tcp:connect(Address, Port, SocketOpts).

%%===================================================================
%%% Init options parsing helpers
%%%===================================================================
-spec get_stream_management_opt(opts()) -> sm_state().
get_stream_management_opt(#{stream_management := false}) ->
    {false, 0, inactive};
get_stream_management_opt(#{manual_ack := true}) ->
    {false, 0, inactive};
get_stream_management_opt(#{stream_management := true, manual_ack := false}) ->
    {true, 0, inactive}.

-spec overwrite_default_opts(GivenOpts :: opts(),
                             DefaultOpts :: opts()) -> opts().
overwrite_default_opts(GivenOpts, DefaultOpts) ->
    maps:merge(DefaultOpts, GivenOpts).

% `ip` option, for backward compatibility reasons, may be crafted from
% `iface` option. Passed `iface` parameter becomes `ip` parameter for
% SocketOpts. However, if `ip` parameter is already defined in `socket_opts`,
% it is not considered:
%
%    #{
%      iface => {1,2,3,4},
%      socket_opts => [{ip, {5,6,7,8}}]
%     }
%
% results in passing {ip, {5,6,7,8}} as gen_tcp parameter
-spec get_socket_opts(opts()) -> [gen_tcp:connect_option()].
get_socket_opts(#{iface := Interface, socket_opts := SocketOpts}) ->
    case proplists:is_defined(ip, SocketOpts) of
        true  -> SocketOpts;
        false -> [{ip, iface_to_ip_address(Interface)} | SocketOpts]
    end;
get_socket_opts(#{socket_opts := SocketOpts}) ->
    SocketOpts.

-spec opts_to_map([proplists:property()] | opts()) -> opts().
opts_to_map(Opts) when is_map(Opts) -> Opts;
opts_to_map(Opts) when is_list(Opts) -> maps:from_list(Opts).
