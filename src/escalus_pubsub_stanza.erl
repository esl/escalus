%%%===================================================================
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%% @doc Stanzas for testing XEP-0060 PubSub
%%% @end
%%%===================================================================

-module(escalus_pubsub_stanza).

-include("escalus.hrl").
-include("escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").

-export([create_node/3, create_node/4,
         request_configuration/3, configure_node/4,
         request_affiliations/3, set_affiliations/4,
         delete_node/3,
         subscribe/3, subscribe/4,
         unsubscribe/3,
         submit_subscription_response/4,
         request_pending_subscriptions/3,
         publish/3, publish/5,
         retract/4,
         request_all_items/3,
         purge_all_items/3,
         retrieve_user_subscriptions/3,
         retrieve_node_subscriptions/3,
         set_subscriptions/4,
         discover_nodes/3]).

-type pubsub_node_id() :: {pep | binary(), binary()}.
-export_type([pubsub_node_id/0]).

-type form_field() :: {Var :: binary(), Value :: binary()}
                      | {Var :: binary(), Type :: binary(), Value :: binary()}.
-type form() :: [form_field()].

%%-----------------------------------------------------------------------------
%% Request construction
%%-----------------------------------------------------------------------------

-spec create_node(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
create_node(User, Id, Node) ->
    create_node(User, Id, Node, []).

-spec create_node(escalus_utils:jid_spec(), binary(), pubsub_node_id(), [{binary(), binary()}]) ->
                         exml:element().
create_node(User, Id, {NodeAddr, NodeName}, ConfigFields) ->
    Elements = [create_node_element(NodeName) | configure_node_form(ConfigFields, undefined)],
    PubSubElement = pubsub_element(Elements, ?NS_PUBSUB),
    iq(<<"set">>, User, Id, NodeAddr, [PubSubElement]).

-spec request_configuration(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
request_configuration(User, Id, {NodeAddr, NodeName}) ->
    Elements = [#xmlel{ name = <<"configure">>,
                        attrs = [{<<"node">>, NodeName}] }],
    PubSubElement = pubsub_element(Elements, ?NS_PUBSUB_OWNER),
    iq(<<"get">>, User, Id, NodeAddr, [PubSubElement]).

-spec configure_node(escalus_utils:jid_spec(), binary(), pubsub_node_id(),
                     [{binary(), binary()}]) ->
                            exml:element().
configure_node(User, Id, {NodeAddr, NodeName}, ConfigFields) ->
    Elements = configure_node_form(ConfigFields, NodeName),
    PubSubElement = pubsub_element(Elements, ?NS_PUBSUB_OWNER),
    iq(<<"set">>, User, Id, NodeAddr, [PubSubElement]).

-spec request_affiliations(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
request_affiliations(User, Id, {NodeAddr, NodeName}) ->
    Elements = [#xmlel{ name = <<"affiliations">>,
                        attrs = [{<<"node">>, NodeName}] }],
    PubSubElement = pubsub_element(Elements, ?NS_PUBSUB_OWNER),
    iq(<<"get">>, User, Id, NodeAddr, [PubSubElement]).

-spec set_affiliations(escalus_utils:jid_spec(), binary(), pubsub_node_id(),
                       [{escalus_utils:jid_spec(), binary()}]) ->
    exml:element().
set_affiliations(User, Id, {NodeAddr, NodeName}, AffChange) ->
    AffList = [ #xmlel{ name = <<"affiliation">>,
                        attrs = [{<<"jid">>, escalus_utils:get_short_jid(U)},
                                 {<<"affiliation">>, A}] }
                || {U, A} <- AffChange ],
    Affiliations = #xmlel{ name = <<"affiliations">>, attrs = [{<<"node">>, NodeName}],
                           children = AffList },
    PubSubElement = pubsub_element([Affiliations], ?NS_PUBSUB_OWNER),
    iq(<<"set">>, User, Id, NodeAddr, [PubSubElement]).

-spec delete_node(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
delete_node(User, Id, {NodeAddr, NodeName}) ->
    Elements = [delete_element(NodeName)],
    PubSubElement = pubsub_element(Elements, ?NS_PUBSUB_OWNER),
    iq(<<"set">>, User, Id, NodeAddr, [PubSubElement]).

-spec subscribe(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
subscribe(User, Id, Node) ->
    subscribe(User, Id, Node, []).

-spec subscribe(escalus_utils:jid_spec(), binary(), pubsub_node_id(), [{binary(), binary()}]) ->
                         exml:element().
subscribe(User, Id, {NodeAddr, NodeName}, ConfigFields) ->
    Elements = [subscribe_element(NodeName, User) | subscribe_options_form(ConfigFields)],
    PubSubElement = pubsub_element(Elements, ?NS_PUBSUB),
    iq(<<"set">>, User, Id, NodeAddr, [PubSubElement]).

-spec unsubscribe(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
unsubscribe(User, Id, {NodeAddr, NodeName}) ->
    Elements = [unsubscribe_element(NodeName, User)],
    PubSubElement = pubsub_element(Elements, ?NS_PUBSUB),
    iq(<<"set">>, User, Id, NodeAddr, [PubSubElement]).

-spec submit_subscription_response(escalus_utils:jid_spec(), binary(), pubsub_node_id(), form()) ->
    exml:element().
submit_subscription_response(User, Id, {NodeAddr, _NodeName}, Form) ->
    Fields = [ encode_form_field(F) || F <- Form ],
    XEl = escalus_stanza:x_data_form(<<"submit">>, Fields),
    Msg = #xmlel{ name = <<"message">>,
                  attrs = [{<<"to">>, NodeAddr}, {<<"id">>, Id}],
                  children = [XEl] },
    escalus_stanza:from(Msg, escalus_utils:get_jid(User)).

-spec request_pending_subscriptions(escalus_utils:jid_spec(), binary(),
                                    pubsub_node_id() | binary()) -> exml:element().
request_pending_subscriptions(User, Id, {NodeAddr, NodeName}) ->
    Fields = [ encode_form_field(<<"pubsub#node">>, NodeName) ],
    Payload = [ escalus_stanza:x_data_form(<<"submit">>, Fields) ],
    Node = <<"http://jabber.org/protocol/pubsub#get-pending">>,
    CommandIQ = escalus_stanza:adhoc_request(Node, Payload),
    escalus_stanza:from(escalus_stanza:set_id(escalus_stanza:to(CommandIQ, NodeAddr), Id), User);
request_pending_subscriptions(User, Id, NodesAddr) ->
    Node = <<"http://jabber.org/protocol/pubsub#get-pending">>,
    CommandIQ = escalus_stanza:adhoc_request(Node, []),
    escalus_stanza:from(escalus_stanza:set_id(escalus_stanza:to(CommandIQ, NodesAddr), Id), User).

-spec publish(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
publish(User, Id, {NodeAddr, NodeName}) ->
    Elements = [publish_element(NodeName, undefined)],
    PubSubElement = pubsub_element(Elements, ?NS_PUBSUB),
    publish_iq(User, PubSubElement, Id, NodeAddr).

-spec publish(escalus_utils:jid_spec(), binary(), exml:element(), binary(), pubsub_node_id()) ->
                     exml:element().
publish(User, ItemId, ContentElement, Id, {NodeAddr, NodeName}) ->
    ItemElement = item_element(ItemId, ContentElement),
    Elements = [publish_element(NodeName, ItemElement)],
    PubSubElement = pubsub_element(Elements, ?NS_PUBSUB),
    publish_iq(User, PubSubElement, Id, NodeAddr).

-spec retract(escalus_utils:jid_spec(), binary(), pubsub_node_id(), binary()) -> exml:element().
retract(User, Id, {NodeAddr, NodeName}, ItemId) ->
    Elements = [retract_item(NodeName, ItemId)],
    PubSubElement = pubsub_element(Elements, ?NS_PUBSUB),
    iq(<<"set">>, User, Id, NodeAddr, [PubSubElement]).

-spec request_all_items(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
request_all_items(User, Id, {NodeAddr, NodeName}) ->
    Elements = [items_element(NodeName)],
    PubSubElement = pubsub_element(Elements, ?NS_PUBSUB),
    iq(<<"get">>, User, Id, NodeAddr, [PubSubElement]).

-spec purge_all_items(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
purge_all_items(User, Id, {NodeAddr, NodeName}) ->
    Elements = [purge_element(NodeName)],
    PubSubElement = pubsub_element(Elements, ?NS_PUBSUB_OWNER),
    iq(<<"set">>, User, Id, NodeAddr, [PubSubElement]).

-spec retrieve_user_subscriptions(escalus_utils:jid_spec(), binary(),
                                  pubsub_node_id() | binary()) -> exml:element().
retrieve_user_subscriptions(User, Id, Node) ->
    {Element, NodeAddr}
    = case Node of
          {NodeAddr0, NodeName} -> {subscriptions_element(NodeName, []), NodeAddr0};
          NodeAddr0 -> {subscriptions_element(), NodeAddr0}
      end,
    PubSubElement = pubsub_element([Element], ?NS_PUBSUB),
    iq(<<"get">>, User, Id, NodeAddr, [PubSubElement]).

-spec retrieve_node_subscriptions(escalus_utils:jid_spec(), binary(), pubsub_node_id()) ->
                                         exml:element().
retrieve_node_subscriptions(User, Id, {NodeAddr, NodeName}) ->
    Elements = [subscriptions_element(NodeName, [])],
    PubSubElement = pubsub_element(Elements, ?NS_PUBSUB_OWNER),
    iq(<<"get">>, User, Id, NodeAddr, [PubSubElement]).

-spec set_subscriptions(escalus_utils:jid_spec(), binary(),
                        [{escalus_utils:jid_spec(), binary()}], pubsub_node_id()) ->
                               exml:element().
set_subscriptions(User, Id, Subscriptions, {NodeAddr, NodeName}) ->
    SubElements = [subscription_element(Jid, SubState) || {Jid, SubState} <- Subscriptions],
    Elements = [subscriptions_element(NodeName, SubElements)],
    PubSubElement = pubsub_element(Elements, ?NS_PUBSUB_OWNER),
    iq(<<"set">>, User, Id, NodeAddr, [PubSubElement]).

-spec discover_nodes(escalus_utils:jid_spec(), binary(), binary() | pubsub_node_id()) ->
                            exml:element().
discover_nodes(User, Id, {NodeAddr, NodeName}) ->
    QueryElement = escalus_stanza:query_el(?NS_DISCO_ITEMS, [{<<"node">>, NodeName}], []),
    iq(<<"get">>, User, Id, NodeAddr, [QueryElement]);
discover_nodes(User, Id, NodeAddr) ->
    QueryElement = escalus_stanza:query_el(?NS_DISCO_ITEMS, [], []),
    iq(<<"get">>, User, Id, NodeAddr, [QueryElement]).

%%-----------------------------------------------------------------------------
%% XML element construction
%%-----------------------------------------------------------------------------

%% Whole stanzas

publish_iq(User, PubSubElement, Id, pep) ->
    iq(<<"set">>, User, Id, [PubSubElement]);
publish_iq(User, PubSubElement, Id, NodeAddr) ->
    iq(<<"set">>, User, Id, NodeAddr, [PubSubElement]).

iq(Type, From, Id, Elements) ->
    Stanza = escalus_stanza:iq(Type, Elements),
    StanzaWithId = escalus_stanza:set_id(Stanza, Id),
    escalus_stanza:from(StanzaWithId, escalus_utils:get_jid(From)).

iq(Type, From, Id, To, Elements) ->
    Stanza = escalus_stanza:iq(To, Type, Elements),
    StanzaWithId = escalus_stanza:set_id(Stanza, Id),
    escalus_stanza:from(StanzaWithId, escalus_utils:get_jid(From)).

%% Form utils

configure_node_form(Fields, NodeName) ->
    optional_form(<<"configure">>, NodeName, <<"node_config">>, Fields).

subscribe_options_form(Fields) ->
    optional_form(<<"options">>, undefined, <<"subscribe_options">>, Fields).

optional_form(_FormName, _NodeName, _Type, []) -> [];
optional_form(FormName, NodeName, Type, Fields) ->
    FormTypeField = form_type_field_element(Type),
    FormFields = [encode_form_field(F) || F <- Fields],
    [form_element(FormName, NodeName, [FormTypeField | FormFields])].

%% Elements

create_node_element(NodeName) ->
    #xmlel{name = <<"create">>, attrs = [{<<"node">>, NodeName}]}.

pubsub_element(Children, NS) ->
    #xmlel{name = <<"pubsub">>,
           attrs = [{<<"xmlns">>, NS}],
           children = Children}.

delete_element(NodeName) ->
    #xmlel{name = <<"delete">>,
           attrs = [{<<"node">>, NodeName}]}.

subscribe_element(NodeName, User) ->
    #xmlel{name = <<"subscribe">>,
           attrs = [{<<"node">>, NodeName},
                    {<<"jid">>, escalus_utils:get_jid(User)}]}.

unsubscribe_element(NodeName, User) ->
    #xmlel{name = <<"unsubscribe">>,
           attrs = [{<<"node">>, NodeName},
                    {<<"jid">>, escalus_utils:get_jid(User)}]}.

publish_element(NodeName, Item) ->
    #xmlel{name = <<"publish">>,
           attrs = [{<<"node">>, NodeName}],
           children = skip_undefined([Item])}.

items_element(NodeName) ->
    #xmlel{name = <<"items">>,
           attrs = [{<<"node">>, NodeName}]}.

item_element(ItemId, ContentElement) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"id">>, ItemId}],
           children = skip_undefined([ContentElement])}.

retract_item(NodeName, ItemId) ->
    #xmlel{name = <<"retract">>,
           attrs = [{<<"node">>, NodeName}],
           children = [#xmlel{ name = <<"item">>,
                               attrs = [{<<"id">>, ItemId}] }]}.

purge_element(NodeName) ->
    #xmlel{name = <<"purge">>,
           attrs = [{<<"node">>, NodeName}]}.

subscriptions_element() ->
    #xmlel{name = <<"subscriptions">>}.

subscriptions_element(NodeName, Children) ->
    #xmlel{name = <<"subscriptions">>,
           attrs = [{<<"node">>, NodeName}],
           children = Children}.

subscription_element(User, SubscriptionState) ->
    #xmlel{name = <<"subscription">>,
           attrs = [{<<"jid">>, escalus_utils:get_jid(User)},
                    {<<"subscription">>, SubscriptionState}]}.

form_element(FormName, NodeName, FieldElements) ->
    #xmlel{name = FormName,
           attrs = skip_undefined([{<<"node">>, NodeName}]),
           children = [#xmlel{name = <<"x">>,
                              attrs = [{<<"xmlns">>, <<"jabber:x:data">>},
                                       {<<"type">>, <<"submit">>}],
                              children = FieldElements}
                      ]}.

form_type_field_element(FormType) ->
    Content = << <<"http://jabber.org/protocol/pubsub#">>/binary, FormType/binary >>,
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, <<"FORM_TYPE">>},
                    {<<"type">>, <<"hidden">>}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = Content}]}]}.

encode_form_field({Var, Content}) ->
    encode_form_field(Var, Content);
encode_form_field({Var, _Type, Content}) ->
    encode_form_field(Var, Content).

encode_form_field(Var, Content) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Var}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = Content}]}]}.

%% Helpers

skip_undefined(L) ->
    lists:filter(fun(undefined) -> false;
                    ({_, undefined}) -> false;
                    (_) -> true
                 end, L).
