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

-include_lib("exml/include/exml.hrl").

%%%===================================================================
%%% Public API
%%%===================================================================

start_stream(Conn, Props) ->
    Server = proplists:get_value(server, Props, <<"localhost">>),
    Host = proplists:get_value(host, Props, Server),
    ok = lxmppc:send(Conn, lxmppc_stanza:stream_start(Server)),
    StreamStart = lxmppc_util:get_stanza(Conn, wait_for_stream),
    %% FIXME: verify StreamStart
    StreamFeatures = lxmppc_util:get_stanza(Conn, wait_for_features),
    %% FIXME: verify StreamFeatures, add props
    Props.

authenticate(Conn, Props) ->
    %% FIXME: as default, select authentication scheme based on stream features
    {M, F} = proplists:get_value(auth, Props, {lxmppc_auth, auth_plain}),
    M:F(Conn, Props),
    lxmppc:reset_parser(Conn),
    lxmppc_session:start_stream(Conn, Props),
    Props.

bind(Conn, Props) ->
    Resource = proplists:get_value(resource, Props, <<"lxmppc-resource">>),
    lxmppc:send(Conn, lxmppc_stanza:bind(Resource)),
    BindReply = lxmppc_util:get_stanza(Conn, bind_reply),
    %% FIXME: verify BindReply, add props
    Props.

session(Conn, Props) ->
    lxmppc:send(Conn, lxmppc_stanza:session()),
    SessionReply = lxmppc_util:get_stanza(Conn, session_reply),
    %% FIXME: verify SessionReply, add props
    Props.
