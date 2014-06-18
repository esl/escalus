%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module abstracting TCP connection to XMPP server
%%% @end
%%%===================================================================

-module(escalus_tcp).
-behaviour(gen_server).

-include_lib("exml/include/exml_stream.hrl").
-include("include/escalus.hrl").

%% API exports
-export([connect/1,
         send/2,
         is_connected/1,
         upgrade_to_tls/2,
         use_zlib/2,
         get_transport/1,
         reset_parser/1,
         get_sm_h/1,
         set_sm_h/2,
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

%% Stream management automation
%%               :: {Auto-ack?,                H,         counting Hs?}.
-type sm_state() :: {boolean(), non_neg_integer(), 'active'|'inactive'}.

-record(state, {owner,
                socket,
                parser,
                sm_state = {true, 0, inactive} :: sm_state(),
                ssl = false      :: boolean(),
                compress = false :: boolean(),
                event_client}).

%%%===================================================================
%%% API
%%%===================================================================

-spec connect({binary(), integer()}) -> {ok, #transport{}}.
connect(Args) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Args, self()], []),
    Transport = gen_server:call(Pid, get_transport),
    {ok, Transport}.

send(#transport{socket = Socket, ssl = true}, Elem) ->
    ssl:send(Socket, exml:to_iolist(Elem));
send(#transport{rcv_pid=Pid, compress = {zlib, {_,Zout}}}, Elem) ->
    gen_server:cast(Pid, {send_compressed, Zout, Elem});
send(#transport{socket = Socket}, Elem) ->
    gen_tcp:send(Socket, exml:to_iolist(Elem)).

is_connected(#transport{rcv_pid = Pid}) ->
    erlang:is_process_alive(Pid).

reset_parser(#transport{rcv_pid = Pid}) ->
    gen_server:cast(Pid, reset_parser).

get_sm_h(#transport{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_sm_h).

set_sm_h(#transport{rcv_pid = Pid}, H) ->
    gen_server:call(Pid, {set_sm_h, H}).

stop(#transport{rcv_pid = Pid}) ->
    try
        gen_server:call(Pid, stop)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            already_stopped
    end.

kill(#transport{rcv_pid = Pid}) ->
    %% Use `kill_connection` to avoid confusion with exit reason `kill`.
    gen_server:call(Pid, kill_connection).

upgrade_to_tls(#transport{socket = Socket, rcv_pid = Pid} = Conn, Props) ->
    Starttls = escalus_stanza:starttls(),
    gen_tcp:send(Socket, exml:to_iolist(Starttls)),
    escalus_connection:get_stanza(Conn, proceed),
    SSLOpts = proplists:get_value(ssl_opts, Props, []),
    gen_server:call(Pid, {upgrade_to_tls, SSLOpts}),
    Conn2 = get_transport(Conn),
    {Props2, _} = escalus_session:start_stream(Conn2, Props),
    {Conn2, Props2}.

use_zlib(#transport{rcv_pid = Pid} = Conn, Props) ->
    escalus_connection:send(Conn, escalus_stanza:compress(<<"zlib">>)),
    Compressed = escalus_connection:get_stanza(Conn, compressed),
    escalus:assert(is_compressed, Compressed),
    gen_server:call(Pid, use_zlib),
    Conn1 = get_transport(Conn),
    {Props2, _} = escalus_session:start_stream(Conn1, Props),
    {Conn1, Props2}.

get_transport(#transport{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_transport).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Args, Owner]) ->
    Host = proplists:get_value(host, Args, <<"localhost">>),
    Port = proplists:get_value(port, Args, 5222),
    EventClient = proplists:get_value(event_client, Args),
    SM = case {proplists:get_value(stream_management, Args, false),
               proplists:get_value(manual_ack, Args, false)}
         of
             {false,_}          -> {false, 0, inactive};
             {_, true}          -> {false, 0, inactive};
             {true,false}       -> {true, 0, inactive}
         end,
    Address = host_to_inet(Host),
    Opts = [binary, {active, once}],
    {ok, Socket} = gen_tcp:connect(Address, Port, Opts),
    {ok, Parser} = exml_stream:new_parser(),
    {ok, #state{owner = Owner,
                socket = Socket,
                parser = Parser,
                sm_state = SM,
                event_client = EventClient}}.

handle_call(get_sm_h, _From, #state{sm_state = {_, H, _}} = State) ->
    {reply, H, State};
handle_call({set_sm_h, H}, _From, #state{sm_state = {A, OldH, S} = SM} = State) ->
    NewState = State#state{sm_state={A, H, S}},
    {reply, {ok, H}, NewState};
handle_call(get_transport, _From, State) ->
    {reply, transport(State), State};
handle_call({upgrade_to_tls, SSLOpts}, _From, #state{socket = Socket} = State) ->
    ssl:start(),
    SSLOpts1 = [{reuse_sessions, true}],
    SSLOpts2 = lists:keymerge(1, lists:keysort(1, SSLOpts),
                              lists:keysort(1, SSLOpts1)),
    {ok, Socket2} = ssl:connect(Socket, SSLOpts2),
    {ok, Parser} = exml_stream:new_parser(),
    {reply, Socket2, State#state{socket = Socket2, parser = Parser, ssl=true}};
handle_call(use_zlib, _, #state{parser = Parser, socket = Socket} = State) ->
    Zin = zlib:open(),
    Zout = zlib:open(),
    ok = zlib:inflateInit(Zin),
    ok = zlib:deflateInit(Zout),
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {reply, Socket, State#state{parser = NewParser,
                                compress = {zlib, {Zin,Zout}}}};
handle_call(kill_connection, _, #state{} = S) ->
    close_compression_streams(S#state.compress),
    {stop, normal, ok, S};
handle_call(stop, _From, #state{} = S) ->
    send_stream_end(S),
    close_compression_streams(S#state.compress),
    wait_until_closed(S#state.socket),
    {stop, normal, ok, S}.

handle_cast({send_compressed, Zout, Elem}, State) ->
    gen_tcp:send(State#state.socket, zlib:deflate(Zout, exml:to_iolist(Elem), sync)),
    {noreply, State};
handle_cast(reset_parser, #state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {noreply, State#state{parser = NewParser}}.

handle_info({tcp, Socket, Data}, State) ->
    inet:setopts(Socket, [{active, once}]),
    handle_data(Socket, Data, State);
handle_info({ssl, Socket, Data}, State) ->
    ssl:setopts(Socket, [{active, once}]),
    handle_data(Socket, Data, State);
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    {stop, normal, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket, ssl = true} = State) ->
    common_terminate(_Reason, State),
    ssl:close(Socket);
terminate(_Reason, #state{socket = Socket} = State) ->
    common_terminate(_Reason, State),
    gen_tcp:close(Socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Helpers
%%%===================================================================
handle_data(Socket, Data, #state{owner = Owner,
                                 parser = Parser,
                                 socket = Socket,
                                 sm_state = SM0,
                                 compress = Compress,
                                 event_client = EventClient} = State) ->
    {ok, NewParser, Stanzas0} =
        case Compress of
            false ->
                exml_stream:parse(Parser, Data);
            {zlib, {Zin,_}} ->
                Decompressed = iolist_to_binary(zlib:inflate(Zin, Data)),
                exml_stream:parse(Parser, Decompressed)
        end,

    {SM1, AckRequests,Stanzas1} = separate_ack_requests(SM0, Stanzas0),
    reply_to_ack_requests(SM1, AckRequests, State),
    NewState = State#state{parser = NewParser, sm_state=SM1},

    lists:foreach(fun(Stanza) ->
        escalus_event:incoming_stanza(EventClient, Stanza),
        Owner ! {stanza, transport(NewState), Stanza}
    end, Stanzas1),

    case [StrEnd || #xmlstreamend{} = StrEnd <- Stanzas1] of
        [] -> {noreply, NewState};
        __ -> {stop, normal, NewState}
    end.

separate_ack_requests({false, H0, A}, Stanzas) ->
    %% Don't keep track of H
    {{false, H0, A}, [], Stanzas};
separate_ack_requests({true, H0, inactive}, Stanzas) ->
    Enabled = [ S || S <- Stanzas, escalus_pred:is_enabled(S)],
    Resumed = [ S || S <- Stanzas, escalus_pred:is_resumed(S)],

    case {length(Enabled),length(Resumed)} of
        %% Enabled SM: set the H param to 0 and activate counter.
        {1,_} -> {{true, 0, active}, [], Stanzas};

        %% Resumed SM: keep the H param and activate counter.
        {_,1} -> {{true, H0, active}, [], Stanzas};

        %% No new SM state: continue as usual
        {0,0} -> {{true, H0, inactive}, [], Stanzas}
    end;
separate_ack_requests({true, H0, active}, Stanzas) ->
    %% Count H and construct appropriate acks
    F = fun(Stanza, {H, Acks, NonAckRequests}) ->
                case escalus_pred:is_ack_request(Stanza) of
                    true -> {H, [make_ack(H)|Acks], NonAckRequests};
                    false -> {H+1, Acks, [Stanza|NonAckRequests]}
                end
        end,
    {H, Acks, Others} = lists:foldl(F, {H0, [], []}, Stanzas),
    {{true, H, active}, lists:reverse(Acks), lists:reverse(Others)}.

make_ack(H) -> {escalus_stanza:sm_ack(H), H}.

reply_to_ack_requests({false,H,A}, _, _) -> {false, H, A};
reply_to_ack_requests({true,H,inactive}, _, _) -> {true, H, inactive};
reply_to_ack_requests({true, H0, active}, Acks, State) ->
    {true,
     lists:foldl(fun({Ack,H}, _) -> raw_send(State, Ack), H end,
                 H0, Acks),
     active}.

raw_send(#state{socket=Socket, ssl=true}, Elem) ->
    ssl:send(Socket, exml:to_iolist(Elem));
raw_send(#state{socket=Socket, compress=true}, Elem) ->
    throw({escalus_tcp, auto_ack_not_implemented_for_compressed_streams});
raw_send(#state{socket=Socket}, Elem) ->
    gen_tcp:send(Socket, exml:to_iolist(Elem)).

common_terminate(_Reason, #state{parser = Parser}) ->
    exml_stream:free_parser(Parser).

transport(#state{socket = Socket,
                 ssl = Ssl,
                 compress = Compress,
                 event_client = EventClient}) ->
    #transport{module = ?MODULE,
               rcv_pid = self(),
               socket = Socket,
               ssl = Ssl,
               compress = Compress,
               event_client = EventClient}.

wait_until_closed(Socket) ->
    receive
        {tcp_closed, Socket} ->
            ok
    after ?WAIT_FOR_SOCKET_CLOSE_TIMEOUT ->
            ok
    end.

-spec host_to_inet(tuple() | atom() | list() | binary())
    -> inet:ip_address() | inet:hostname().
host_to_inet({_,_,_,_} = IP4) -> IP4;
host_to_inet({_,_,_,_,_,_,_,_} = IP6) -> IP6;
host_to_inet(Address) when is_list(Address) orelse is_atom(Address) -> Address;
host_to_inet(BAddress) when is_binary(BAddress) -> binary_to_list(BAddress).

close_compression_streams(false) ->
    ok;
close_compression_streams({zlib, {Zin, Zout}}) ->
    try
        ok = zlib:inflateEnd(Zin),
        ok = zlib:deflateEnd(Zout)
    catch
        error:data_error -> ok
    after
        ok = zlib:close(Zin),
        ok = zlib:close(Zout)
    end.

send_stream_end(#state{socket = Socket, ssl = Ssl, compress = Compress}) ->
    StreamEnd = escalus_stanza:stream_end(),
    case {Ssl, Compress} of
        {true, _} ->
            ssl:send(Socket, exml:to_iolist(StreamEnd));
        {false, {zlib, {_, Zout}}} ->
            gen_tcp:send(Socket, zlib:deflate(Zout,
                                              exml:to_iolist(StreamEnd),
                                              finish));
        {false, false} ->
            gen_tcp:send(Socket, exml:to_iolist(StreamEnd))
    end.
