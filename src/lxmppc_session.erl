%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module providing basic session manipulation
%%% @end
%%%===================================================================

-module(lxmppc_session).
-export([start_stream/2,
         authenticate/2,
         bind/2,
         session/2]).

-define(TIMEOUT, 1000).

-include_lib("exml/include/exml.hrl").

%%%===================================================================
%%% Public API
%%%===================================================================

start_stream(Conn, Props) ->
    Server = proplists:get_value(server, Props, <<"localhost">>),
    Host = proplists:get_value(host, Props, Server),
    ok = lxmppc:send(Conn, lxmppc_stanza:stream_start(Host)),
    StreamStart = get_stanza(Conn, wait_for_stream),
    %% FIXME: verify StreamStart
    StreamFeatures = get_stanza(Conn, wait_for_features),
    %% FIXME: verify StreamFeatures, add props
    Props.

authenticate(Conn, Props) ->
    Username = proplists:get_value(username, Props, <<"anonymous">>),
    Password = proplists:get_value(password, Props, <<"">>),
    %% FIXME: select authentication scheme based on stream features
    ok = lxmppc:send(Conn, lxmppc_stanza:auth_plain(Username, Password)),
    AuthReply = get_stanza(Conn, auth_reply),
    case AuthReply#xmlelement.name of
        <<"success">> ->
            lxmppc:reset_stream(Conn),
            start_stream(Conn, Props);
        <<"failure">> ->
            throw({auth_failed, Username, AuthReply})
    end.

bind(Conn, Props) ->
    Resource = proplists:get_value(resource, Props, <<"lxmppc-resource">>),
    lxmppc:send(Conn, lxmppc_stanza:bind(Resource)),
    BindReply = get_stanza(Conn, bind_reply),
    %% FIXME: verify BindReply, add props
    Props.

session(Conn, Props) ->
    lxmppc:send(Conn, lxmppc_stanza:session()),
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
