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
         can_use_stream_management/2,
         session/2]).

%% New style connection initiation
-export([start_stream/3,
         maybe_use_ssl/3,
         maybe_use_compression/3,
         maybe_stream_management/3,
         maybe_stream_resumption/3,
         stream_management/3,
         stream_resumption/3,
         authenticate/3,
         bind/3,
         session/3]).

-include_lib("exml/include/exml.hrl").
-define(DEFAULT_RESOURCE, <<"escalus-default-resource">>).

%%%===================================================================
%%% Public API
%%%===================================================================


start_stream(Conn, Props) ->
    Host = proplists:get_value(host, Props, <<"localhost">>),
    XMLNS = case proplists:get_value(endpoint, Props) of
                {server, _} -> <<"jabber:server">>;
                _ -> <<"jabber:client">>
            end,
    case proplists:get_value(transport, Props, tcp) of
        ws ->
            ok = escalus_connection:send(Conn, escalus_stanza:ws_stream_start(
                                         Host));
        _ ->
            ok = escalus_connection:send(Conn, escalus_stanza:stream_start(Host,XMLNS))
    end,
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
        {required, false} -> error("Client requires StartTLS "
                                   "but server doesn't offer it");
        {false, _ } -> false;
        {optional, true} -> true;
        _ -> false
    end.

can_use_compression(Props, Features) ->
    false /= proplists:get_value(compression, Props, false) andalso
    false /= proplists:get_value(compression, Features).

can_use_stream_management(Props, Features) ->
    false /= proplists:get_value(stream_management, Props, false) andalso
    false /= proplists:get_value(stream_management, Features).

%%%===================================================================
%%% New style connection initiation
%%%===================================================================

start_stream(Conn, Props, [] = _Features) ->
    {Props1, Features} = start_stream(Conn, Props),
    {Conn, Props1, Features}.

maybe_use_ssl(Conn, Props, Features) ->
    case use_ssl(Props, Features) of
        true ->
            {Conn1, Props1} = starttls(Conn, Props),
            {Conn1, Props1, Features};
        false ->
            {Conn, Props, Features}
    end.

maybe_use_compression(Conn, Props, Features) ->
    case can_use_compression(Props, Features) of
        true ->
            {Conn1, Props1} = compress(Conn, Props),
            {Conn1, Props1, Features};
        false ->
            {Conn, Props, Features}
    end.

maybe_stream_management(Conn, Props, Features) ->
    case can_use_stream_management(Props, Features) of
        true ->
            stream_management(Conn, Props, Features);
        false ->
            {Conn, Props, Features}
    end.

stream_management(Conn, Props, Features) ->
    escalus_connection:send(Conn, escalus_stanza:enable_sm()),
    Enabled = escalus_connection:get_stanza(Conn, stream_management),
    true = escalus_pred:is_enabled(Enabled),
    {Conn, Props, Features}.

maybe_stream_resumption(Conn, Props, Features) ->
    case can_use_stream_management(Props, Features) of
        true ->
            stream_resumption(Conn, Props, Features);
        false ->
            {Conn, Props, Features}
    end.

stream_resumption(Conn, Props, Features) ->
    escalus_connection:send(Conn, escalus_stanza:enable_sm([resume])),
    Enabled = escalus_connection:get_stanza(Conn, stream_resumption),
    true = escalus_pred:is_enabled([resume], Enabled),
    SMID = exml_query:attr(Enabled, <<"id">>),
    {Conn, [{smid, SMID} | Props], Features}.

authenticate(Conn, Props, Features) ->
    {Conn, authenticate(Conn, Props), Features}.

bind(Conn, Props, Features) ->
    {Conn, bind(Conn, Props), Features}.

session(Conn, Props, Features) ->
    {Conn, session(Conn, Props), Features}.

%%%===================================================================
%%% Helpers
%%%===================================================================

get_stream_features(Features) ->
    [{compression, get_compression(Features)},
     {starttls, get_starttls(Features)},
     {stream_management, get_stream_management(Features)}].

get_compression(Features) ->
    case exml_query:subelement(Features, <<"compression">>) of
        #xmlel{children = MethodEls} ->
            [exml_query:cdata(MethodEl) || MethodEl <- MethodEls];
        _ -> false
    end.

get_starttls(Features) ->
    case exml_query:subelement(Features, <<"starttls">>) of
        undefined ->
            false;
        _ -> true
    end.

get_stream_management(Features) ->
    case exml_query:subelement(Features, <<"sm">>) of
        undefined ->
            false;
        _ -> true
    end.
