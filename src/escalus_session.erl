%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module providing basic session manipulation
%%% @end
%%%===================================================================

-module(escalus_session).
-export([start_stream/1,
         authenticate/1,
         starttls/1,
         bind/1,
         compress/1,
         use_ssl/2,
         can_use_amp/2,
         can_use_compression/2,
         can_use_stream_management/2,
         session/1]).

%% New style connection initiation
-export([start_stream/2,
         stream_features/2,
         maybe_use_ssl/2,
         maybe_use_carbons/2,
         maybe_use_compression/2,
         maybe_stream_management/2,
         maybe_stream_resumption/2,
         stream_management/2,
         stream_resumption/2,
         authenticate/2,
         bind/2,
         session/2]).

-export([send_presence_available/1,
         send_presence_unavailable/1]).

%% Public Types
-export_type([feature/0,
              features/0,
              step/0,
              step_state/0]).

-type feature() :: {atom(), any()}.
-type features() :: [feature()].
-define(CONNECTION_STEP, (escalus_connection:client(), features()) -> step_state()).
-define(CONNECTION_STEP_SIG(Module), Module?CONNECTION_STEP).
-type step() :: fun(?CONNECTION_STEP).
-type step_state() :: {escalus_connection:client(), features()}.

%% Some shorthands
-type client() :: escalus_connection:client().
-type user_spec() :: escalus_users:user_spec().

-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include("escalus_xmlns.hrl").
-include("escalus.hrl").
-define(DEFAULT_RESOURCE, <<"escalus-default-resource">>).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec start_stream(client()) -> client().
start_stream(Client = #client{props = Props}) ->
    StreamStartRep = escalus_connection:start_stream(Client),
    Client#client{props = maybe_store_stream_id(StreamStartRep, Props)}.

-spec starttls(client()) -> client().
starttls(Client) ->
    escalus_connection:send(Client, escalus_stanza:starttls()),
    escalus_connection:get_stanza(Client, proceed),
    escalus_connection:upgrade_to_tls(Client),
    start_stream(Client).

-spec authenticate(client()) -> client().
authenticate(Client = #client{props = Props}) ->
    %% NOTE: we could select authentication scheme based on stream features,
    %% but as a default we use plain, as it incurrs lower load and better logs (no hashing)
    %% for common setups. If a different mechanism is required then it should be configured on the
    %% user specification.
    {M, F} = proplists:get_value(auth, Props, {escalus_auth, auth_plain}),
    PropsAfterAuth = case apply(M, F, [Client, Props]) of
                         ok -> Props;
                         {ok, P} when is_list(P) -> P
                     end,
    escalus_connection:reset_parser(Client),
    Client1 = escalus_session:start_stream(Client#client{props = PropsAfterAuth}),
    escalus_session:stream_features(Client1, []),
    Client1.

-spec bind(client()) -> client().
bind(Client = #client{props = Props0}) ->
    Resource = proplists:get_value(resource, Props0, ?DEFAULT_RESOURCE),
    BindStanza = escalus_stanza:bind(Resource),

    escalus_connection:send(Client, BindStanza),
    BindReply = escalus_connection:get_stanza(Client, bind_reply),
    escalus:assert(is_bind_result, BindReply),

    case exml_query:path(BindReply, [{element, <<"bind">>}, {element, <<"jid">>}, cdata]) of
        undefined ->
            Client;
        JID ->
            Props1 = lists:keystore(resource, 1, Props0,
                                    {resource, escalus_utils:get_resource(JID)}),
            Props2 = lists:keyreplace(username, 1, Props1,
                                     {username, escalus_utils:get_username(JID)}),
            escalus_connection:maybe_set_jid(Client#client{props = Props2})
    end.

-spec compress(client()) -> client().
compress(Client = #client{props = Props}) ->
    case proplists:get_value(compression, Props, false) of
        false ->
            Client;
        <<"zlib">> ->
            use_zlib(Client)
    end.

use_zlib(Client) ->
    escalus_connection:send(Client, escalus_stanza:compress(<<"zlib">>)),
    Compressed = escalus_connection:get_stanza(Client, compressed),
    escalus:assert(is_compressed, Compressed),
    escalus_connection:use_zlib(Client),
    start_stream(Client).

-spec session(client()) -> client().
session(Client) ->
    escalus_connection:send(Client, escalus_stanza:session()),
    SessionReply = escalus_connection:get_stanza(Client, session_reply),
    escalus:assert(is_iq_result, SessionReply),
    Client.

-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

-spec send_presence_unavailable(escalus:client()) -> ok.
send_presence_unavailable(Client) ->
    Pres = escalus_stanza:presence(<<"unavailable">>),
    escalus_connection:send(Client, Pres).

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

-spec ?CONNECTION_STEP_SIG(start_stream).
start_stream(Client, [] = _Features) ->
    Client1 = start_stream(Client),
    {Client1, []}.

-spec ?CONNECTION_STEP_SIG(stream_features).
stream_features(Client = #client{props = Props}, [] = _Features) ->
    StreamFeatures = escalus_connection:get_stanza(Client, wait_for_features),
    Transport = proplists:get_value(transport, Props, tcp),
    IsLegacy = proplists:get_value(wslegacy, Props, false),
    assert_stream_features(StreamFeatures, Transport, IsLegacy),
    {Client, get_stream_features(StreamFeatures)}.

-spec ?CONNECTION_STEP_SIG(maybe_use_ssl).
maybe_use_ssl(Client = #client{props = Props}, Features) ->
    case use_ssl(Props, Features) of
        true ->
            Client1 = starttls(Client),
            {Client2, Features2} = stream_features(Client1, []),
            {Client2, Features2};
        false ->
            {Client, Features}
    end.

-spec ?CONNECTION_STEP_SIG(maybe_use_carbons).
maybe_use_carbons(Client = #client{props = Props}, Features) ->
    case can_use_carbons(Props, Features) of
        true ->
            use_carbons(Client, Features);
        false ->
            {Client, Features}
    end.

-spec ?CONNECTION_STEP_SIG(use_carbons).
use_carbons(Client, Features) ->
    escalus_connection:send(Client, escalus_stanza:carbons_enable()),
    Result = escalus_connection:get_stanza(Client, carbon_iq_response),
    escalus:assert(is_iq, [<<"result">>], Result),
    {Client, Features}.

-spec ?CONNECTION_STEP_SIG(maybe_use_compression).
maybe_use_compression(Client = #client{props = Props}, Features) ->
    case can_use_compression(Props, Features) of
        true ->
            Client1 = compress(Client),
            stream_features(Client1, []);
        false ->
            {Client, Features}
    end.

-spec ?CONNECTION_STEP_SIG(maybe_stream_management).
maybe_stream_management(Client = #client{props = Props}, Features) ->
    case can_use_stream_management(Props, Features) of
        true ->
            stream_management(Client, Features);
        false ->
            {Client, Features}
    end.

-spec ?CONNECTION_STEP_SIG(stream_management).
stream_management(Client, Features) ->
    escalus_connection:send(Client, escalus_stanza:enable_sm()),
    Enabled = escalus_connection:get_stanza(Client, stream_management),
    true = escalus_pred:is_sm_enabled(Enabled),
    {Client, Features}.

-spec ?CONNECTION_STEP_SIG(maybe_stream_resumption).
maybe_stream_resumption(Client = #client{props = Props}, Features) ->
    case can_use_stream_management(Props, Features) of
        true ->
            stream_resumption(Client, Features);
        false ->
            {Client, Features}
    end.

-spec ?CONNECTION_STEP_SIG(stream_resumption).
stream_resumption(Client = #client{props = Props}, Features) ->
    escalus_connection:send(Client, escalus_stanza:enable_sm([resume])),
    Enabled = escalus_connection:get_stanza(Client, stream_resumption),
    true = escalus_pred:is_sm_enabled([resume], Enabled),
    SMID = exml_query:attr(Enabled, <<"id">>),
    {Client#client{props = [{smid, SMID} | Props]}, Features}.

-spec ?CONNECTION_STEP_SIG(authenticate).
authenticate(Client, Features) ->
    {authenticate(Client), Features}.

-spec ?CONNECTION_STEP_SIG(bind).
bind(Client, Features) ->
    {bind(Client), Features}.

-spec ?CONNECTION_STEP_SIG(session).
session(Client, Features) ->
    {session(Client), Features}.

%%%===================================================================
%%% Helpers
%%%===================================================================

assert_stream_features(StreamFeatures, Transport, IsLegacy) ->
    case {StreamFeatures, Transport, IsLegacy} of
        {#xmlel{name = <<"features">>}, escalus_ws, false} ->
            ok;
        {#xmlel{name = <<"features">>}, escalus_ws, true} ->
            error("<features> with legacy WebSocket");
        {#xmlel{name = <<"stream:features">>}, escalus_ws, false} ->
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
     {client_state_indication, get_client_state_indication(Features)},
     {sasl_mechanisms, get_sasl_mechanisms(Features)},
     {caps, get_server_caps(Features)}].

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

-spec get_sasl_mechanisms(exml:element()) -> [exml:element() | binary()].
get_sasl_mechanisms(Features) ->
    exml_query:paths(Features, [{element, <<"mechanisms">>},
                                {element, <<"mechanism">>}, cdata]).

-spec get_server_caps(exml:element()) -> undefined | map().
get_server_caps(Features) ->
    case exml_query:subelement(Features, <<"c">>) of
        #xmlel{attrs = Attrs} ->
            Attrs;
        _ ->
            undefined
    end.


-spec stream_start_to_element(exml_stream:element()) -> exml:element().
stream_start_to_element(#xmlel{name = <<"open">>} = Open) -> Open;
stream_start_to_element(#xmlstreamstart{name = Name, attrs = Attrs}) ->
    #xmlel{name = Name, attrs = Attrs, children = []}.

maybe_store_stream_id(StreamStartResponse, Props) ->
    case exml_query:attr(stream_start_to_element(StreamStartResponse),
                         <<"id">>, no_id) of
        no_id -> Props;
        ID when is_binary(ID) ->
            lists:keystore(stream_id, 1, Props, {stream_id, ID})
    end.
