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
         can_use_amp/2,
         can_use_compression/2,
         can_use_stream_management/2,
         session/2]).

%% New style connection initiation
-export([start_stream/3,
         stream_features/3,
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
-export_type([feature/0,
              features/0,
              step/0,
              step_state/0]).

-type feature() :: {atom(), boolean()}.
-type features() :: [feature()].
-define(CONNECTION_STEP, (escalus_connection:client(),
                          escalus_users:user_spec(),
                          features()) -> step_state()).
-type step() :: fun(?CONNECTION_STEP).
-type step_state() :: {escalus_connection:client(),
                       escalus_users:user_spec(),
                       features()}.

%% Some shorthands
-type client() :: escalus_connection:client().
-type user_spec() :: escalus_users:user_spec().

-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include("escalus_xmlns.hrl").
-define(DEFAULT_RESOURCE, <<"escalus-default-resource">>).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec start_stream(client(), user_spec()) -> {user_spec(), features()}.
start_stream(Conn, Props) ->
    {server, Server} = lists:keyfind(server, 1, Props),
    NS = proplists:get_value(stream_ns, Props, <<"jabber:client">>),
    Transport = proplists:get_value(transport, Props, tcp),
    IsLegacy = proplists:get_value(wslegacy, Props, false),
    StreamStartReq = case {Transport, IsLegacy} of
                         {ws, false} -> escalus_stanza:ws_open(Server);
                         _ -> escalus_stanza:stream_start(Server, NS)
                     end,
    ok = escalus_connection:send(Conn, StreamStartReq),
    StreamStartRep = escalus_connection:get_stanza(Conn, wait_for_stream),
    assert_stream_start(StreamStartRep, Transport, IsLegacy),
    StreamID = case exml_query:attr(stream_start_to_element(StreamStartRep),
                                    <<"id">>, no_id)
               of
                   no_id -> error({invalid_response, no_id});
                   ID when is_binary(ID) -> ID
               end,
    %% TODO: deprecate 2-tuple return value
    %% To preserve the previous interface we still return a 2-tuple,
    %% but it's guaranteed that the features will be empty.
    {lists:keystore(stream_id, 1, Props, {stream_id, StreamID}), []}.

-spec starttls(client(), user_spec()) -> {client(), user_spec()}.
starttls(Conn, Props) ->
    escalus_tcp:upgrade_to_tls(Conn, Props).

-spec authenticate(client(), user_spec()) -> user_spec().
authenticate(Conn, Props) ->
    %% FIXME: as default, select authentication scheme based on stream features
    {M, F} = proplists:get_value(auth, Props, {escalus_auth, auth_plain}),
    PropsAfterAuth = case M:F(Conn, Props) of
                         ok -> Props;
                         {ok, P} when is_list(P) -> P
                     end,
    escalus_connection:reset_parser(Conn),
    {Props1, []} = escalus_session:start_stream(Conn, PropsAfterAuth),
    escalus_session:stream_features(Conn, Props1, []),
    Props1.

-spec bind(client(), user_spec()) -> user_spec().
bind(Conn, Props) ->
    Resource = proplists:get_value(resource, Props, ?DEFAULT_RESOURCE),
    escalus_connection:send(Conn, escalus_stanza:bind(Resource)),
    BindReply = escalus_connection:get_stanza(Conn, bind_reply),
    escalus:assert(is_bind_result, BindReply),
    case proplists:get_value(auth_method, Props) of
        <<"SASL-ANON">> ->
            JID = exml_query:path(BindReply, [{element, <<"bind">>}, {element, <<"jid">>}, cdata]),
            TMPUsername = escalus_utils:get_username(JID),
            lists:keyreplace(username, 1, Props, {username, TMPUsername});
        _ ->
            Props
    end.

-spec compress(client(), user_spec()) -> {client(), user_spec()}.
compress(Conn, Props) ->
    case proplists:get_value(compression, Props, false) of
        false ->
            {Conn, Props};
        <<"zlib">> ->
            escalus_tcp:use_zlib(Conn, Props)
        %% TODO: someday maybe lzw too
    end.

-spec session(client(), user_spec()) -> user_spec().
session(Conn, Props) ->
    escalus_connection:send(Conn, escalus_stanza:session()),
    SessionReply = escalus_connection:get_stanza(Conn, session_reply),
    escalus:assert(is_iq_result, SessionReply),
    Props.

-spec use_ssl(user_spec(), features()) -> boolean().
use_ssl(Props, Features) ->
    UserNeedsSSL = proplists:get_value(starttls, Props, false),
    StreamAllowsSSL = proplists:get_value(starttls, Features),
    case {UserNeedsSSL, StreamAllowsSSL} of
        {required, true} -> true;
        {required, false} -> error("Client requires StartTLS "
                                   "but server doesn't offer it");
        {false, _ } -> false;
        {optional, true} -> true;
        _ -> false
    end.

-spec can_use_compression(user_spec(), features()) -> boolean().
can_use_compression(Props, Features) ->
    can_use(compression, Props, Features).

-spec can_use_stream_management(user_spec(), features()) -> boolean().
can_use_stream_management(Props, Features) ->
    can_use(stream_management, Props, Features).

can_use_carbons(Props, _Features) ->
    false /= proplists:get_value(carbons, Props, false).

-spec can_use_amp(user_spec(), features()) -> boolean().
can_use_amp(_Props, Features) ->
    false /= proplists:get_value(advanced_message_processing, Features).

can_use(Feature, Props, Features) ->
    false /= proplists:get_value(Feature, Props, false) andalso
    false /= proplists:get_value(Feature, Features).

%%%===================================================================
%%% New style connection initiation
%%%===================================================================

-spec start_stream/3 :: ?CONNECTION_STEP.
start_stream(Conn, Props, [] = _Features) ->
    {Props1, []} = start_stream(Conn, Props),
    {Conn, Props1, []}.

-spec stream_features/3 :: ?CONNECTION_STEP.
stream_features(Conn, Props, [] = _Features) ->
    StreamFeatures = escalus_connection:get_stanza(Conn, wait_for_features),
    Transport = proplists:get_value(transport, Props, tcp),
    IsLegacy = proplists:get_value(wslegacy, Props, false),
    assert_stream_features(StreamFeatures, Transport, IsLegacy),
    {Conn, Props, get_stream_features(StreamFeatures)}.

-spec maybe_use_ssl/3 :: ?CONNECTION_STEP.
maybe_use_ssl(Conn, Props, Features) ->
    case use_ssl(Props, Features) of
        true ->
            {Conn1, Props1} = starttls(Conn, Props),
            {Conn2, Props2, Features2} = stream_features(Conn1, Props1, []),
            {Conn2, Props2, Features2};
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
            {Conn2, Props2, Features2} = stream_features(Conn1, Props1, []),
            {Conn2, Props2, Features2};
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
    true = escalus_pred:is_sm_enabled(Enabled),
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
    true = escalus_pred:is_sm_enabled([resume], Enabled),
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

assert_stream_start(StreamStartRep, Transport, IsLegacy) ->
    case {StreamStartRep, Transport, IsLegacy} of
        {#xmlel{name = <<"open">>}, ws, false} ->
            ok;
        {#xmlel{name = <<"open">>}, ws, true} ->
            error("<open/> with legacy WebSocket",
                  [StreamStartRep]);
        {#xmlstreamstart{}, ws, false} ->
            error("<stream:stream> with non-legacy WebSocket",
                  [StreamStartRep]);
        {#xmlstreamstart{}, _, _} ->
            ok;
        _ ->
            error("Not a valid stream start", [StreamStartRep])
    end.

assert_stream_features(StreamFeatures, Transport, IsLegacy) ->
    case {StreamFeatures, Transport, IsLegacy} of
        {#xmlel{name = <<"features">>}, ws, false} ->
            ok;
        {#xmlel{name = <<"features">>}, ws, true} ->
            error("<features> with legacy WebSocket");
        {#xmlel{name = <<"stream:features">>}, ws, false} ->
            error("<stream:features> with non-legacy WebSocket",
                  [StreamFeatures]);
        {#xmlel{name = <<"stream:features">>}, _, _} ->
            ok;
        _ ->
           error(
             lists:flatten(
               io_lib:format(
                 "Expected stream features, got ~p",
                 [StreamFeatures])))
    end.

-spec get_stream_features(exml:element()) -> features().
get_stream_features(Features) ->
    [{compression, get_compression(Features)},
     {starttls, get_starttls(Features)},
     {stream_management, get_stream_management(Features)},
     {advanced_message_processing, get_advanced_message_processing(Features)},
     {client_state_indication, get_client_state_indication(Features)}]
    ++ get_sasl_mechanisms(Features).

-spec get_compression(exml:element()) -> boolean().
get_compression(Features) ->
    case exml_query:subelement(Features, <<"compression">>) of
        #xmlel{children = MethodEls} ->
            [exml_query:cdata(MethodEl) || MethodEl <- MethodEls];
        _ -> false
    end.

-spec get_starttls(exml:element()) -> boolean().
get_starttls(Features) ->
    undefined =/= exml_query:subelement(Features, <<"starttls">>).

-spec get_stream_management(exml:element()) -> boolean().
get_stream_management(Features) ->
    undefined =/= exml_query:subelement(Features, <<"sm">>).

-spec get_advanced_message_processing(exml:element()) -> boolean().
get_advanced_message_processing(Features) ->
    undefined =/= exml_query:subelement(Features, <<"amp">>).

-spec get_client_state_indication(exml:element()) -> boolean().
get_client_state_indication(Features) ->
    undefined =/= exml_query:subelement(Features, <<"csi">>).

-spec get_sasl_mechanisms(exml:element()) -> features().
get_sasl_mechanisms(Features) ->
    MechCDataPath = [{element, <<"mechanisms">>}, {element, <<"mechanism">>}, cdata],
    [ {mechanism_to_auth_function(Mech), true}
      || Mech <- exml_query:paths(Features, MechCDataPath) ].

mechanism_to_auth_function(<<"PLAIN">>)       -> auth_plain;
mechanism_to_auth_function(<<"DIGEST-MD5">>)  -> auth_digest_md5;
%% TODO: no auth_anonymous in escalus_auth!
mechanism_to_auth_function(<<"ANONYMOUS">>)   -> auth_anonymous;
mechanism_to_auth_function(<<"EXTERNAL">>)    -> auth_sasl_external;
mechanism_to_auth_function(<<"SCRAM-SHA-1">>) -> auth_sasl_scram_sha1;
mechanism_to_auth_function(<<"X-OAUTH">>)     -> auth_sasl_oauth.

-spec stream_start_to_element(exml_stream:start()) -> exml:element().
stream_start_to_element(#xmlstreamstart{name = Name, attrs = Attrs}) ->
    #xmlel{name = Name, attrs = Attrs, children = []}.
