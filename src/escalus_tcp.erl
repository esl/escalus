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
         stop/1,
         kill/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(WAIT_FOR_SOCKET_CLOSE_TIMEOUT, 200).
-define(SERVER, ?MODULE).

-record(state, {owner,
                socket,
                parser,
                ssl = false,
                compress = false,
                event_client,
                on_reply,
                on_request}).

%%%===================================================================
%%% API
%%%===================================================================

-spec connect({binary(), integer()}) -> {ok, #transport{}}.
connect(Args) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Args, self()], []),
    Transport = gen_server:call(Pid, get_transport),
    {ok, Transport}.

send(#transport{rcv_pid=Pid} = Transport, Elem) ->
    gen_server:cast(Pid, {send, Transport, Elem}).

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
    OnReplyFun = proplists:get_value(on_reply, Args, fun(_) -> ok end),
    OnRequestFun = proplists:get_value(on_request, Args, fun(_) -> ok end),
    OnConnectFun = proplists:get_value(on_connect, Args, fun(_) -> ok end),
    Address = host_to_inet(Host),
    Opts = [binary, {active, once}],
    {ok, Socket} = do_connect(Address, Port, Opts, OnConnectFun),
    {ok, Parser} = exml_stream:new_parser(),
    {ok, #state{owner = Owner,
                socket = Socket,
                parser = Parser,
                event_client = EventClient,
                on_reply = OnReplyFun,
                on_request = OnRequestFun}}.

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

handle_cast({send, #transport{socket = Socket, ssl = Ssl, compress = Compress}, 
             Elem}, #state{on_request = OnRequestFun} = State) ->
    Response =
        case {Ssl, Compress} of
    {true, _} ->
        ssl:send(Socket, exml:to_iolist(Elem));
    {false, {zlib, {_,Zout}}} ->
        gen_tcp:send(State#state.socket, zlib:deflate(Zout, exml:to_iolist(Elem), sync));
    {false, false} ->
        gen_tcp:send(Socket, exml:to_iolist(Elem))
    end, 
    OnRequestFun(Response),
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
                                 compress = Compress,
                                 event_client = EventClient,
                                 on_reply = OnReplyFun} = State) ->
    OnReplyFun({erlang:byte_size(Data)}),
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

do_connect(Address, Port, Opts, OnConnectFun) ->
    TimeB = os:timestamp(),
    Reply = gen_tcp:connect(Address, Port, Opts),
    TimeA = os:timestamp(),
    ConnectionTime = timer:now_diff(TimeA, TimeB),
    case Reply of
        {ok, Socket} ->
            OnConnectFun({ok, Socket, ConnectionTime});
        {error, _} ->
            OnConnectFun(Reply)
    end,
    Reply.
