%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Module abstracting Websockets over TCP connection to XMPP server
%%% @end
%%%===================================================================

-module(escalus_ws).
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
         stop/1]).

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

-record(state, {owner, socket, parser, compress = false, event_client}).

%%%===================================================================
%%% API
%%%===================================================================

-spec connect({binary(), integer()}) -> {ok, #transport{}}.
connect(Args) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Args, self()], []),
    Transport = gen_server:call(Pid, get_transport),
    {ok, Transport}.

send(#transport{socket = Socket, rcv_pid=Pid, compress = {zlib, {_,Zout}}}, Elem) ->
    gen_server:cast(Pid, {send_compressed, Zout, Elem});
send(#transport{socket = Socket, rcv_pid=Pid}, Elem) ->
    gen_server:cast(Pid, {send, exml:to_iolist(Elem)}).

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

upgrade_to_tls(_, _) ->
    throw(starttls_not_supported).

use_zlib(#transport{rcv_pid = Pid} = Conn, Props) ->
    escalus_connection:send(Conn, escalus_stanza:compress(<<"zlib">>)),
    Compressed = escalus_connection:get_stanza(Conn, compressed),
    %% FIXME: verify Compressed
    gen_server:call(Pid, use_zlib),
    Conn1 = get_transport(Conn),
    {Props2, _} = escalus_session:start_ws_stream(Conn1, Props),
    {Conn1, Props2}.

get_transport(#transport{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_transport).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Args, Owner]) ->
    Host = proplists:get_value(host, Args, <<"localhost">>),
    Port = proplists:get_value(port, Args, 5222),
    WSPath = proplists:get_value(wspath, Args, <<"ws-xmpp">>),
    EventClient = proplists:get_value(event_client, Args),
    HostStr = binary_to_list(Host),
    {ok, Socket} = wsecli:start(HostStr, Port, WSPath, anon),
    Pid = self(),
    wsecli:on_open(Socket, fun() -> Pid ! opened end),
    wsecli:on_error(Socket, fun(Reason) -> Pid ! {error, Reason} end),
    wsecli:on_message(Socket, fun(Type, Data) -> Pid ! {Type, Data} end),
    wsecli:on_close(Socket, fun(_Msg) -> Pid ! tcp_closed end),
    wait_for_socket_start(),
    {ok, Parser} = exml_stream:new_parser(toplevel),
    {ok, #state{owner = Owner,
                socket = Socket,
                parser = Parser,
                event_client = EventClient}}.

handle_call(get_transport, _From, State) ->
    {reply, transport(State), State};
handle_call(use_zlib, _, #state{parser = Parser, socket = Socket} = State) ->
    Zin = zlib:open(),
    Zout = zlib:open(),
    ok = zlib:inflateInit(Zin),
    ok = zlib:deflateInit(Zout),
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {reply, Socket, State#state{parser = NewParser,
                                compress = {zlib, {Zin,Zout}}}};
handle_call(stop, _From, #state{socket = Socket,
                                compress = Compress} = State) ->
    StreamEnd = escalus_stanza:ws_stream_end(),
    case Compress of
        {zlib, {Zin, Zout}} ->
            try
                ok = zlib:inflateEnd(Zin)
            catch
                error:data_error -> ok
            end,
            ok = zlib:close(Zin),
            wsecli:send(Socket, zlib:deflate(Zout,
                                              exml:to_iolist(StreamEnd),
                                              finish)),
            ok = zlib:deflateEnd(Zout),
            ok = zlib:close(Zout);
        false ->
            wsecli:send(Socket, exml:to_iolist(StreamEnd))
    end,
    wait_until_closed(),
    {stop, normal, ok, State}.

handle_cast({send_compressed, Zout, Elem}, State) ->
    wsecli:send(State#state.socket, zlib:deflate(Zout, exml:to_iolist(Elem), sync)),
    {noreply, State};
handle_cast({send, Data}, State) ->
    wsecli:send(State#state.socket, Data),
    {noreply, State};
handle_cast(reset_parser, #state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {noreply, State#state{parser = NewParser}}.

handle_info(tcp_closed, State) ->
    {stop, normal, State};
handle_info({error, Reason}, State) ->
    {stop, Reason, State};
handle_info({text, Data}, State) ->
    handle_info({binary, list_to_binary(lists:flatten(Data))}, State);
handle_info({binary, Data}, State) ->
    handle_data(Data, State);
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket} = State) ->
    common_terminate(_Reason, State),
    wsecli:stop(Socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Helpers
%%%===================================================================
handle_data(Data, State = #state{owner = Owner,
                                 parser = Parser,
                                 socket = Socket,
                                 compress = Compress,
                                 event_client = EventClient}) ->
    {ok, NewParser, Stanzas} =
        case Compress of
            false ->
                exml_stream:parse(Parser, Data);
            {zlib, {Zin,_}} ->
                Decompressed = iolist_to_binary(zlib:inflate(Zin, Data)),
                exml_stream:parse(Parser, Decompressed)
        end,
    NewState = State#state{parser = NewParser},
    lists:foreach(fun(Stanza) ->
        escalus_event:incoming_stanza(EventClient, Stanza),
        Owner ! {stanza, transport(NewState), Stanza}
    end, Stanzas),
    case [StrEnd || #xmlstreamend{} = StrEnd <- Stanzas] of
        [] -> {noreply, NewState};
        __ -> {stop, normal, NewState}
    end.

common_terminate(_Reason, #state{parser = Parser}) ->
    exml_stream:free_parser(Parser).

transport(#state{socket = Socket,
                 compress = Compress,
                 event_client = EventClient}) ->
    #transport{module = ?MODULE,
               socket = Socket,
               ssl = undefined,
               compress = Compress,
               rcv_pid = self(),
               event_client = EventClient}.

wait_until_closed() ->
    receive
        tcp_closed ->
            ok
    after ?WAIT_FOR_SOCKET_CLOSE_TIMEOUT ->
            ok
    end.

wait_for_socket_start() ->
    receive
        opened ->
            ok
    after ?HANDSHAKE_TIMEOUT ->
            throw(handshake_timeout)
    end.
