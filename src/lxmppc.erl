%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module providing basic client functionality
%%% @end
%%%===================================================================
-module(lxmppc).

-include_lib("exml/include/exml_stream.hrl").
-include("lxmppc.hrl").

%% High-level API
-export([start/1,
         stop/1]).

%% Low-level API
-export([connect/1,
         send/2,
         reset_parser/1,
         is_connected/1]).

%%%===================================================================
%%% Public API
%%%===================================================================

start(Props0) ->
    case connect(Props0) of
        {ok, Conn, Props1} ->
            try
                Props2 = lxmppc_session:start_stream(Conn, Props1),
                Props3 = lxmppc_session:authenticate(Conn, Props2),
                Props4 = lxmppc_session:bind(Conn, Props3),
                Props5 = lxmppc_session:session(Conn, Props4),
                {ok, Conn, Props5}
            catch Error ->
                Mod = Conn#transport.module,
                Mod:stop(Conn),
                {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

connect(Props) ->
    Transport = proplists:get_value(transport, Props, tcp),
    Server = proplists:get_value(server, Props, <<"localhost">>),
    Host = proplists:get_value(host, Props, Server),
    NewProps = lists:keystore(host, 1, Props, {host, Host}),
    Mod = get_module(Transport),
    {ok, Conn} = Mod:connect(NewProps),
    {ok, Conn, Props}.

send(#transport{module = lxmppc_socket_tcp, socket = Socket}, Elem) ->
    Data = exml:to_iolist(Elem),
    gen_tcp:send(Socket, Data).

reset_parser(#transport{module = Mod} = Transport) ->
    Mod:reset_parser(Transport).

is_connected(#transport{rcv_pid = Pid}) ->
    erlang:is_process_alive(Pid).

stop(#transport{module = Mod} = Transport) ->
    Mod:stop(Transport).

%%%===================================================================
%%% Helpers
%%%===================================================================

get_module(tcp) ->
    lxmppc_socket_tcp.
