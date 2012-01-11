%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
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
-export([connect/2,
         send/2,
         reset_stream/1,
         is_connected/1]).

-define(TIMEOUT, 1000).

%%%===================================================================
%%% Public API
%%%===================================================================

start(Props) ->
    %% TODO: change getting defaults when implementing BOSH
    Type = proplists:get_value(transport, Props, tcp),
    Server = proplists:get_value(server, Props, <<"localhost">>),
    Host = proplists:get_value(host, Props, Server),
    Port = proplists:get_value(port, Props, 5222),
    try
        NewProps = lists:keystore(host, 1, Props, {host, Host}),
        start_connect(Type, Host, Port, NewProps)
    catch Error ->
        {error, Error}
    end.


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
%%% Client start implementation
%%%===================================================================

start_connect(Type, Host, Port, Props0) ->
    case connect(Type, {Host, Port}) of
        {ok, Conn} ->
            ok = send(Conn, lxmppc_stanza:stream_start(Host)),
            Props1 = wait_for_stream(Conn, Props0),
            Props2 = authenticate(Conn, Props1),
            Props3 = bind(Conn, Props2),
            Props4 = session(Conn, Props3),
            {ok, Conn, Props4};
        {error, Err} ->
            throw({failed_to_connect, Err})
    end.

wait_for_stream(Conn, Props) ->
    StreamStart = get_stanza(Conn, wait_for_stream),
    %% FIXME: verify StreamStart
    StreamFeatures = get_stanza(Conn, wait_for_features),
    %% FIXME: verify StreamFeatures, add props
    Props.

authenticate(Conn, Props) ->
    Username = proplists:get_value(username, Props, <<"anonymous">>),
    Password = proplists:get_value(password, Props, <<"">>),
    Host = proplists:get_value(host, Props),
    %% FIXME: select authentication scheme based on stream features
    ok = send(Conn, lxmppc_stanza:auth_plain(Username, Password)),
    AuthReply = get_stanza(Conn, auth_reply),
    case AuthReply#xmlelement.name of
        <<"success">> ->
            reset_stream(Conn),
            ok = send(Conn, lxmppc_stanza:stream_start(Host)),
            wait_for_stream(Conn, Props);
        <<"failure">> ->
            throw({auth_failed, AuthReply})
    end.

bind(Conn, Props) ->
    Resource = proplists:get_value(resource, Props, <<"lxmppc-resource">>),
    send(Conn, lxmppc_stanza:bind(Resource)),
    BindReply = get_stanza(Conn, bind_reply),
    %% FIXME: verify BindReply, add props
    Props.

session(Conn, Props) ->
    send(Conn, lxmppc_stanza:session()),
    SessionReply = get_stanza(Conn, session_reply),
    %% FIXME: verify SessionReply, add props
    Props.

%%%===================================================================
%%% Helpers
%%%===================================================================

get_stanza(Conn, Name) ->
    receive
        {stanza, Conn, Stanza} ->
            Stanza
    after ?TIMEOUT ->
            throw({timeout, Name})
    end.

get_module(tcp) ->
    lxmppc_socket_tcp.
