%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module abstracting TCP connection to XMPP server
%%% @end
%%%===================================================================
-module(lxmppc_socket_tcp).

%% API exports
-export([connect/1, reset_parser/1, stop/1]).

%% spawn exports
-export([start_receiver/3]).

-include_lib("exml/include/exml_stream.hrl").
-include("lxmppc.hrl").

-record(state, {owner, socket, parser}).

-define(CONNECT_TIMEOUT, 5000).

%%%===================================================================
%%% API exports
%%%===================================================================

-spec connect({binary(), integer()}) -> {ok, #transport{}}.
connect(Args) ->
    Host = proplists:get_value(host, Args, <<"localhost">>),
    Port = proplists:get_value(port, Args, 5222),
    {Pid, Mon} =
            erlang:spawn_monitor(?MODULE,
                                 start_receiver,
                                 [self(), Host, Port]),
    receive
        {connected, Pid, Socket} ->
            {ok, #transport{
                    module = ?MODULE,
                    socket = Socket,
                    rcv_pid = Pid
                 }};
        {'DOWN', Mon, process, Pid, Reason} ->
            {error, {couldnt_connect, {Host, Port}, Reason}}
    after ?CONNECT_TIMEOUT ->
        {error, {couldnt_connect, {Host, Port}, timeout}}
    end.

reset_parser(#transport{rcv_pid = Pid}) ->
    Pid ! reset_parser,
    ok.

-spec stop(#transport{}) -> ok.
stop(#transport{rcv_pid=Pid}) ->
    Pid ! stop,
    ok.

%%%===================================================================
%%% spawn exports
%%%===================================================================

start_receiver(Owner, Host, Port) ->
    HostStr = binary_to_list(Host),
    Opts = [binary, {active, once}],
    {ok, Socket} = gen_tcp:connect(HostStr, Port, Opts),
    {ok, Parser} = exml_stream:new_parser(),
    Owner ! {connected, self(), Socket},
    try
        loop(#state{owner = Owner,
                    socket = Socket,
                    parser = Parser})
    after
        exml_stream:free_parser(Parser),
        gen_tcp:close(Socket)
    end.

%%%===================================================================
%%% receiver implementation
%%%===================================================================

loop(#state{owner = Owner, socket = Socket, parser = Parser} = State) ->
    % TODO: add handling for receiving </stream:stream>
    receive
        {tcp, Socket, Data} ->
            inet:setopts(Socket, [{active, once}]),
            {ok, NewParser, Stanzas} = exml_stream:parse(Parser, Data),
            lists:foreach(fun(Stanza) ->
                Owner ! stanza_msg(Socket, Stanza)
            end, Stanzas),
            loop(State#state{parser = NewParser});
        reset_parser ->
            {ok, NewParser} = exml_stream:reset_parser(Parser),
            loop(State#state{parser = NewParser});
        stop ->
            %% TODO: send </stream:stream>
            stopped;
        {tcp_closed, Socket} ->
            stopped;
        Other ->
            %% TODO: implement
            error_logger:info_msg("FIXME: unhandled message", [Other]),
            exit({bad_msg, Other})
    end.

stanza_msg(Socket, Stanza) ->
    %% FIXME: fugly, will break when field is added to #transport
    Transport = #transport{module = ?MODULE,
                           socket = Socket,
                           rcv_pid = self()},
    {stanza, Transport, Stanza}.
