%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module providing basic session manipulation
%%% @end
%%%===================================================================

-module(escalus_session).
-export([start_stream/2,
         authenticate/2,
         starttls/2,
         bind/2,
         compress/2,
         use_ssl/2,
         can_use_compression/2,
         session/2]).

-include_lib("exml/include/exml.hrl").
-define(DEFAULT_RESOURCE, <<"escalus-default-resource">>).

%%%===================================================================
%%% Public API
%%%===================================================================

start_stream(Conn, Props) ->
    Server = proplists:get_value(server, Props, <<"localhost">>),
    Host = proplists:get_value(host, Props, Server),
    XMLNS = case proplists:get_value(endpoint, Props) of
                {server, _} -> <<"jabber:server">>;
                _ -> <<"jabber:client">>
            end,
    ok = escalus_connection:send(Conn, escalus_stanza:stream_start(
                                         Server, XMLNS)),
    StreamStart = escalus_connection:get_stanza(Conn, wait_for_stream),
    %% FIXME: verify StreamStart
    StreamFeatures = escalus_connection:get_stanza(Conn, wait_for_features),
    %% FIXME: verify StreamFeatures
    {Props, get_stream_features(StreamFeatures)}.

starttls(Conn, Props) ->
    escalus_tcp:upgrade_to_tls(Conn, Props).

authenticate(Conn, Props) ->
    %% FIXME: as default, select authentication scheme based on stream features
    {M, F} = proplists:get_value(auth, Props, {escalus_auth, auth_plain}),
    M:F(Conn, Props),
    escalus_connection:reset_parser(Conn),
    escalus_session:start_stream(Conn, Props),
    Props.

bind(Conn, Props) ->
    Resource = proplists:get_value(resource, Props, ?DEFAULT_RESOURCE),
    escalus_connection:send(Conn, escalus_stanza:bind(Resource)),
    BindReply = escalus_connection:get_stanza(Conn, bind_reply),
    %% FIXME: verify BindReply, add props
    Props.

compress(Conn, Props) ->
    case proplists:get_value(compression, Props, false) of
        false ->
            {Conn, Props};
        <<"zlib">> ->
            escalus_tcp:use_zlib(Conn, Props)
        %% TODO: someday maybe lzw too
    end.

session(Conn, Props) ->
    escalus_connection:send(Conn, escalus_stanza:session()),
    SessionReply = escalus_connection:get_stanza(Conn, session_reply),
    %% FIXME: verify SessionReply, add props
    Props.

use_ssl(Props, Features) ->
    UserNeedSSL = proplists:get_value(ssl, Props, false),
    StreamAllowSSL = proplists:get_value(starttls, Features),
    case {UserNeedSSL, StreamAllowSSL} of
        {required, true} -> true;
        {required, false} -> {error, "Client requires StartTLS "
                                     "but server doesn't offer it"};
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

get_stream_features(Features) ->
    [
        {compression, get_compression(Features)},
        {starttls, get_starttls(Features)}
    ].

get_compression(Features) ->
    case exml_query:subelement(Features, <<"compression">>) of
        #xmlelement{children = MethodEls} ->
            [ exml_query:cdata(MethodEl) || MethodEl <- MethodEls ];
        _ -> false
    end.

get_starttls(Features) ->
    case exml_query:subelement(Features, <<"starttls">>) of
        undefined ->
            false;
        _ -> true
    end.
