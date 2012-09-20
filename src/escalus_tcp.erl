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

-record(state, {owner, socket, parser, ssl = false, compress = false}).

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
send(#transport{socket = Socket, rcv_pid=Pid, compress = {zlib, {_,Zout}}}, Elem) ->
    gen_server:cast(Pid, {send_compressed, Zout, Elem});
send(#transport{socket = Socket}, Elem) ->
    gen_tcp:send(Socket, exml:to_iolist(Elem)).

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
    %% FIXME: verify Compressed
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
    HostStr = binary_to_list(Host),
    Opts = [binary, {active, once}],
    {ok, Socket} = gen_tcp:connect(HostStr, Port, Opts),
    {ok, Parser} = exml_stream:new_parser(),
    {ok, #state{owner = Owner,
                socket = Socket,
                parser = Parser}}.

handle_call(get_transport, _From, State) ->
    {reply, transport(State), State};
handle_call({upgrade_to_tls, SSLOpts}, _From, #state{socket = Socket} = State) ->
    ssl:start(),
    SSLOpts1 = [{protocol, tlsv1}, {reuse_sessions, true}],
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
handle_call(stop, _From, #state{socket = Socket, ssl = Ssl,
                                compress = Compress} = State) ->
    StreamEnd = escalus_stanza:stream_end(),
    case {Ssl, Compress} of
        {true, _} ->
            ssl:send(Socket, exml:to_iolist(StreamEnd));
        {false, {zlib, {Zin, Zout}}} ->
            try
                ok = zlib:inflateEnd(Zin)
            catch
                error:data_error -> ok
            end,
            ok = zlib:close(Zin),
            gen_tcp:send(Socket, zlib:deflate(Zout,
                                              exml:to_iolist(StreamEnd),
                                              finish)),
            ok = zlib:deflateEnd(Zout),
            ok = zlib:close(Zout);
        {false, false} ->
            gen_tcp:send(Socket, exml:to_iolist(StreamEnd))
    end,
    wait_until_closed(State#state.socket),
    {stop, normal, ok, State}.

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
                                 compress = Compress} = State) ->
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
        Owner ! {stanza, transport(NewState), Stanza}
    end, Stanzas),
    case [StrEnd || #xmlstreamend{} = StrEnd <- Stanzas] of
        [] -> {noreply, NewState};
        __ -> {stop, normal, NewState}
    end.

common_terminate(_Reason, #state{parser = Parser}) ->
    exml_stream:free_parser(Parser).

transport(#state{socket = Socket, ssl = Ssl, compress = Compress}) ->
    #transport{module = ?MODULE,
               socket = Socket,
               ssl = Ssl,
               compress = Compress,
               rcv_pid = self()}.

wait_until_closed(Socket) ->
    receive
        {tcp_closed, Socket} ->
            ok
    after ?WAIT_FOR_SOCKET_CLOSE_TIMEOUT ->
            ok
    end.
