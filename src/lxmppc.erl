%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Module abstracting away various socket implementations
%%% @end
%%%===================================================================
-module(lxmppc).

-include_lib("exml/include/exml_stream.hrl").
-include("lxmppc.hrl").

-export([connect/2,
         send/2,
         reset_stream/1,
         is_connected/1,
         stop/1]).

%%%===================================================================
%%% Public API
%%%===================================================================

connect(Type, Args) ->
    Mod = get_module(Type),
    Mod:connect(Args).

send(#transport{module = lxmppc_socket_tcp, socket = Socket}, Elem) ->
    Data = exml:to_iolist(Elem),
    gen_tcp:send(Socket, Data).

reset_stream(#transport{module = Mod} = Transport) ->
    Mod:reset_stream(Transport).

is_connected(#transport{rcv_pid = Pid}) ->
    erlang:is_process_alive(Pid).

stop(#transport{module = Mod} = Transport) ->
    Mod:stop(Transport).

%%%===================================================================
%%% Helpers
%%%===================================================================

get_module(tcp) ->
    lxmppc_socket_tcp.
