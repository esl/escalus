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
         maybe_use_carbons/3,
         maybe_use_compression/3,
         maybe_stream_management/3,
         maybe_stream_resumption/3,
         stream_management/3,
         stream_resumption/3,
         authenticate/3,
         bind/3,
         session/3]).

%% Public Types
-type feature() :: {atom(), boolean()}.
-export_type([feature/0]).

-type features() :: [feature()].
-export_type([features/0]).

-define(CONNECTION_STEP, (escalus_connection:transport(),
                          escalus_users:spec(),
                          features()) -> step_state()).
-type step() :: fun(?CONNECTION_STEP).
-export_type([step/0]).

-type step_state() :: {escalus_connection:transport(),
                       escalus_users:spec(),
                       features()}.
-export_type([step_state/0]).

-include_lib("exml/include/exml_stream.hrl").
-define(DEFAULT_RESOURCE, <<"escalus-default-resource">>).

%%%===================================================================
%%% Public API
%%%===================================================================

start_stream(Conn, Props) ->
    {server, Server} = lists:keyfind(server, 1, Props),
    XMLNS = case proplists:get_value(endpoint, Props) of
                {server, _} -> <<"jabber:server">>;
                _ -> <<"jabber:client">>
            end,
    StreamStartReq = case {proplists:get_value(transport, Props, tcp),
                           proplists:get_value(wslegacy, Props, false)} of
                         {ws, false} -> escalus_stanza:ws_open(Server);
                         _ -> escalus_stanza:stream_start(Server, XMLNS)
                     end,
    ok = escalus_connection:send(Conn, StreamStartReq),
    StreamStartRep = escalus_connection:get_stanza(Conn, wait_for_stream),
    case StreamStartRep of
       #xmlstreamend{} ->
           error("Stream terminated by server");
       #xmlstreamstart{} ->
           ok
    end,
    StreamFeatures = escalus_connection:get_stanza(Conn, wait_for_features),
    case StreamFeatures of
       #xmlel{name = <<"stream:features">>} ->
           ok;
       _ ->
           error(
             lists:flatten(
               io_lib:format(
                 "Expected stream features, got ~p",
                 [StreamFeatures])))
    end,
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

-spec can_use_compression(escalus_users:spec(), features()) -> boolean().
can_use_compression(Props, Features) ->
    false /= proplists:get_value(compression, Props, false) andalso
    false /= proplists:get_value(compression, Features).

can_use_stream_management(Props, Features) ->
    false /= proplists:get_value(stream_management, Props, false) andalso
    false /= proplists:get_value(stream_management, Features).

can_use_carbons(Props, Features) ->
    false /= proplists:get_value(carbons, Props, false).


%%%===================================================================
%%% New style connection initiation
%%%===================================================================

-spec start_stream/3 :: ?CONNECTION_STEP.
start_stream(Conn, Props, [] = _Features) ->
    {Props1, Features} = start_stream(Conn, Props),
    {Conn, Props1, Features}.

-spec maybe_use_ssl/3 :: ?CONNECTION_STEP.
maybe_use_ssl(Conn, Props, Features) ->
    case use_ssl(Props, Features) of
        true ->
            {Conn1, Props1} = starttls(Conn, Props),
            {Conn1, Props1, Features};
        false ->
            {Conn, Props, Features}
    end.

-spec maybe_use_carbons/3 :: ?CONNECTION_STEP.
maybe_use_carbons(Conn, Props, Features) ->
    case can_use_carbons(Props, Features) of
        true ->
            use_carbons(Conn, Props, Features);
        false ->
            {Conn, Props, Features}
    end.

-spec use_carbons/3 :: ?CONNECTION_STEP.
use_carbons(Conn, Props, Features) ->
    escalus_connection:send(Conn, escalus_stanza:carbons_enable()),
    Result = escalus_connection:get_stanza(Conn, carbon_iq_response),
    escalus:assert(is_iq, [<<"result">>], Result),
    {Conn, Props, Features}.

-spec maybe_use_compression/3 :: ?CONNECTION_STEP.
maybe_use_compression(Conn, Props, Features) ->
    case can_use_compression(Props, Features) of
        true ->
            {Conn1, Props1} = compress(Conn, Props),
            {Conn1, Props1, Features};
        false ->
            {Conn, Props, Features}
    end.

-spec maybe_stream_management/3 :: ?CONNECTION_STEP.
maybe_stream_management(Conn, Props, Features) ->
    case can_use_stream_management(Props, Features) of
        true ->
            stream_management(Conn, Props, Features);
        false ->
            {Conn, Props, Features}
    end.

-spec stream_management/3 :: ?CONNECTION_STEP.
stream_management(Conn, Props, Features) ->
    escalus_connection:send(Conn, escalus_stanza:enable_sm()),
    Enabled = escalus_connection:get_stanza(Conn, stream_management),
    true = escalus_pred:is_enabled(Enabled),
    {Conn, Props, Features}.

-spec maybe_stream_resumption/3 :: ?CONNECTION_STEP.
maybe_stream_resumption(Conn, Props, Features) ->
    case can_use_stream_management(Props, Features) of
        true ->
            stream_resumption(Conn, Props, Features);
        false ->
            {Conn, Props, Features}
    end.

-spec stream_resumption/3 :: ?CONNECTION_STEP.
stream_resumption(Conn, Props, Features) ->
    escalus_connection:send(Conn, escalus_stanza:enable_sm([resume])),
    Enabled = escalus_connection:get_stanza(Conn, stream_resumption),
    true = escalus_pred:is_enabled([resume], Enabled),
    SMID = exml_query:attr(Enabled, <<"id">>),
    {Conn, [{smid, SMID} | Props], Features}.

-spec authenticate/3 :: ?CONNECTION_STEP.
authenticate(Conn, Props, Features) ->
    {Conn, authenticate(Conn, Props), Features}.

-spec bind/3 :: ?CONNECTION_STEP.
bind(Conn, Props, Features) ->
    {Conn, bind(Conn, Props), Features}.

-spec session/3 :: ?CONNECTION_STEP.
session(Conn, Props, Features) ->
    {Conn, session(Conn, Props), Features}.

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec get_stream_features(xmlterm()) -> features().
get_stream_features(Features) ->
    [{compression, get_compression(Features)},
     {starttls, get_starttls(Features)},
     {stream_management, get_stream_management(Features)}].

-spec get_compression(xmlterm()) -> boolean().
get_compression(Features) ->
    case exml_query:subelement(Features, <<"compression">>) of
        #xmlel{children = MethodEls} ->
            [exml_query:cdata(MethodEl) || MethodEl <- MethodEls];
        _ -> false
    end.

-spec get_starttls(xmlterm()) -> boolean().
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
