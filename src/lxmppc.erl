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
                CanUseSSL = lxmppc_session:can_use_ssl(Props2),
                CanUseCompression = lxmppc_session:can_use_compression(Props2),
                {Conn1, Props3} = if
                    CanUseSSL ->
                        lxmppc_session:starttls(Conn0, Props2);
                    CanUseCompression ->
                        lxmppc_session:compress(Conn0, Props2);
                    true ->
                        {Conn0, Props2}
                end,
                try
                    Props5 = lxmppc_session:authenticate(Conn1, Props3),
                    Props6 = lxmppc_session:bind(Conn1, Props5),
                    Props7 = lxmppc_session:session(Conn1, Props6),
                    {ok, Conn1, Props7}
                catch Error0 ->
                    handle_start_error(Error0, Conn1)
                end
            catch Error1 ->
                handle_start_error(Error1, Conn0)
            end;
        {error, Error} ->
            {error, Error}
    end.

handle_start_error(Error, Conn) ->
    Mod = Conn#transport.module,
    Mod:stop(Conn),
    {error, Error}.

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
