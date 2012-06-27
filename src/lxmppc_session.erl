%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module providing basic session manipulation
%%% @end
%%%===================================================================

-module(lxmppc_session).
-export([start_stream/2,
         authenticate/2,
         starttls/2,
         bind/2,
         compress/2,
         can_use_ssl/2,
         can_use_compression/2,
         session/2]).

-include_lib("exml/include/exml.hrl").

-compile([export_all]).

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
    %% FIXME: verify StreamFeatures
    {Props, get_stream_features(StreamFeatures, Props)}.

starttls(Conn, Props) ->
    lxmppc_socket_tcp:upgrade_to_tls(Conn, Props).

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

compress(Conn, Props) ->
    case proplists:get_value(compression, Props, false) of
        false ->
            {Conn, Props};
        <<"zlib">> ->
            lxmppc_socket_tcp:use_zlib(Conn, Props)
        %% TODO: someday maybe lzw too
    end.

session(Conn, Props) ->
    lxmppc:send(Conn, lxmppc_stanza:session()),
    SessionReply = lxmppc_util:get_stanza(Conn, session_reply),
    %% FIXME: verify SessionReply, add props
    Props.

can_use_ssl(Props, Features) ->
    UserNeedSSL = proplists:get_value(ssl, Props, false),
    StreamAllowSSL = proplists:get_value(starttls, Features),
    case {UserNeedSSL, StreamAllowSSL} of
        {required, true} -> true;
        {required, false} -> false; %% FIXME raise error
        {false, _ } -> false;
        {optional, true} -> true;
        _ -> false
    end.

can_use_compression(Props, Features) ->
    false /= proplists:get_value(compression, Props, false) andalso
    false /= proplists:get_value(compression, Features).
%%%===================================================================
%%% Helpers
%%%===================================================================

get_stream_features(Features, Props) ->
    [
        {compression, get_compression(Features)},
        {starttls, get_starttls(Features)}
    ].

get_compression(Features) ->
    case exml_query:subelement(Features, <<"compression">>) of
        #xmlelement{body = MethodEls} ->
            Methods = [ exml_query:cdata(MethodEl) || MethodEl <- MethodEls ];
        _ -> false
    end.
get_starttls(Features) ->
    case exml_query:subelement(Features, <<"starttls">>) of
        undefined ->
            false;
        _ -> true
    end.
