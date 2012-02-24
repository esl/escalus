%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module abstracting TCP connection to XMPP server
%%% @end
%%%===================================================================

-module(lxmppc_socket_tcp).
-behaviour(gen_server).

-include_lib("exml/include/exml_stream.hrl").
-include("lxmppc.hrl").

%% API exports
-export([connect/1, reset_parser/1, stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(WAIT_FOR_SOCKET_CLOSE_TIMEOUT, 200).
-define(SERVER, ?MODULE).

-record(state, {owner, socket, parser}).

%%%===================================================================
%%% API
%%%===================================================================

-spec connect({binary(), integer()}) -> {ok, #transport{}}.
connect(Args) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Args, self()], []),
    Transport = gen_server:call(Pid, get_transport),
    {ok, Transport}.

reset_parser(#transport{rcv_pid = Pid}) ->
    gen_server:cast(Pid, reset_parser).

stop(#transport{rcv_pid = Pid}) ->
    try
        gen_server:call(Pid, stop)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            already_stopped
    end.

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
handle_call(stop, _From, #state{socket = Socket} = State) ->
    StreamEnd = lxmppc_stanza:stream_end(),
    gen_tcp:send(Socket, exml:to_iolist(StreamEnd)),
    wait_until_closed(State#state.socket),
    {stop, normal, ok, State}.

handle_cast(reset_parser, #state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {noreply, State#state{parser = NewParser}}.

handle_info({tcp, Socket, Data}, #state{owner = Owner,
                                        parser = Parser,
                                        socket = Socket} = State) ->
    inet:setopts(Socket, [{active, once}]),
    {ok, NewParser, Stanzas} = exml_stream:parse(Parser, Data),
    NewState = State#state{parser = NewParser},
    lists:foreach(fun(Stanza) ->
        Owner ! {stanza, transport(NewState), Stanza}
    end, Stanzas),
    case [StrEnd || #xmlstreamend{} = StrEnd <- Stanzas] of
        [] -> {noreply, NewState};
        __ -> {stop, normal, NewState}
    end;
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    {stop, normal, State}.

terminate(_Reason, #state{parser = Parser, socket = Socket}) ->
    exml_stream:free_parser(Parser),
    gen_tcp:close(Socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Helpers
%%%===================================================================

transport(#state{socket = Socket}) ->
    #transport{module = ?MODULE,
               socket = Socket,
               rcv_pid = self()}.

wait_until_closed(Socket) ->
    receive
        {tcp_closed, Socket} ->
            ok
    after ?WAIT_FOR_SOCKET_CLOSE_TIMEOUT ->
            ok
    end.
