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
        {ok, Conn0, Props1} ->
            try
                Props2 = lxmppc_session:start_stream(Conn0, Props1),
                {Conn,Props21} = lxmppc_session:starttls(Conn0, Props2),
                Props3 = lxmppc_session:authenticate(Conn, Props21),
                Props4 = lxmppc_session:bind(Conn, Props3),
                Props5 = lxmppc_session:session(Conn, Props4),
                {ok, Conn, Props5}
            catch Error ->
                Mod = Conn0#transport.module,
                Mod:stop(Conn0),
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
    {ok, Conn, NewProps}.

send(#transport{module = Mod} = Transport, Elem) ->
    Mod:send(Transport, Elem).

reset_parser(#transport{module = Mod} = Transport) ->
    Mod:reset_parser(Transport).

is_connected(#transport{module = Mod} = Transport) ->
    Mod:is_connected(Transport).

stop(#transport{module = Mod} = Transport) ->
    Mod:stop(Transport).

%%%===================================================================
%%% Helpers
%%%===================================================================

get_module(tcp) ->
    lxmppc_socket_tcp.
