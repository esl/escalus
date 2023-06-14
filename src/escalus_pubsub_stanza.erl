%%%===================================================================
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%% @doc Stanzas for testing XEP-0060 PubSub
%%% @end
%%%===================================================================

-module(escalus_pubsub_stanza).

-include("escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-export([
         discover_nodes/3,

         create_node/3, create_node/4, delete_node/3,

         get_configuration/3, set_configuration/4,
         get_default_configuration/3,

         get_affiliations/3, set_affiliations/4,

         subscribe/3, subscribe/4, unsubscribe/3,
         submit_subscription_response/4,
         get_pending_subscriptions/3,
         get_user_subscriptions/3,
         get_subscription_options/3,
         set_subscription_options/4,
         get_node_subscriptions/3,
         set_subscriptions/4,
         pubsub_owner_iq/5,
         pubsub_iq/5,
         iq/5,

         publish/3, publish/4, publish/5,
         publish_with_options/4, publish_with_options/5, publish_with_options/6,

         retract/4,
         get_items/4,
         get_all_items/3,
         get_item/4,
         purge_all_items/3
        ]).

-type pubsub_node_id() :: {pep | binary(), binary()}.
-export_type([pubsub_node_id/0]).

-type publish_options() :: [{binary(), binary()}].

-type form_field() :: {Var :: binary(), Value :: binary()}
                      | {Var :: binary(), Type :: binary(), Value :: binary()}.
-type form() :: [form_field()].

%%-----------------------------------------------------------------------------
%% Request construction
%%-----------------------------------------------------------------------------

%% ---------------- disco ----------------

-spec discover_nodes(escalus_utils:jid_spec(), binary(), binary() | pubsub_node_id()) ->
                            exml:element().
discover_nodes(User, Id, {NodeAddr, NodeName}) ->
    QueryElement = escalus_stanza:query_el(?NS_DISCO_ITEMS, [{<<"node">>, NodeName}], []),
    iq(<<"get">>, User, Id, NodeAddr, [QueryElement]);
discover_nodes(User, Id, NodeAddr) ->
    QueryElement = escalus_stanza:query_el(?NS_DISCO_ITEMS, [], []),
    iq(<<"get">>, User, Id, NodeAddr, [QueryElement]).

%% ---------------- create & delete ----------------

-spec create_node(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
create_node(User, Id, Node) ->
    create_node(User, Id, Node, []).

-spec create_node(escalus_utils:jid_spec(), binary(), pubsub_node_id(), [{binary(), binary()}]) ->
                         exml:element().
create_node(User, Id, {NodeAddr, NodeName}, ConfigFields) ->
    Elements = [create_node_element(NodeName) | configure_node_form(ConfigFields, undefined)],
    pubsub_iq(<<"set">>, User, Id, NodeAddr, Elements).

-spec delete_node(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
delete_node(User, Id, {NodeAddr, NodeName}) ->
    Elements = [delete_element(NodeName)],
    pubsub_owner_iq(<<"set">>, User, Id, NodeAddr, Elements).

%% ---------------- configuration ----------------

-spec get_configuration(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
get_configuration(User, Id, {NodeAddr, NodeName}) ->
    Elements = [#xmlel{ name = <<"configure">>,
                        attrs = [{<<"node">>, NodeName}] }],
    pubsub_owner_iq(<<"get">>, User, Id, NodeAddr, Elements).

-spec set_configuration(escalus_utils:jid_spec(), binary(),
                        pubsub_node_id(), [{binary(), binary()}]) ->
    exml:element().
set_configuration(User, Id, {NodeAddr, NodeName}, ConfigFields) ->
    Elements = configure_node_form(ConfigFields, NodeName),
    pubsub_owner_iq(<<"set">>, User, Id, NodeAddr, Elements).

-spec get_default_configuration(escalus_utils:jid_spec(), binary(),
                                escalus_utils:jid_spec()) -> exml:element().
get_default_configuration(User, Id, NodeAddr) ->
    Elements = [default_element()],
    pubsub_owner_iq(<<"get">>, User, Id, NodeAddr, Elements).

%% ---------------- affiliations ----------------

-spec get_affiliations(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
get_affiliations(User, Id, {NodeAddr, NodeName}) ->
    Elements = [#xmlel{ name = <<"affiliations">>,
                        attrs = [{<<"node">>, NodeName}] }],
    pubsub_owner_iq(<<"get">>, User, Id, NodeAddr, Elements).

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
    pubsub_owner_iq(<<"set">>, User, Id, NodeAddr, [Affiliations]).

%% ---------------- subscriptions ----------------

-spec subscribe(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
subscribe(User, Id, Node) ->
    subscribe(User, Id, Node, []).

-spec subscribe(escalus_utils:jid_spec(), binary(), pubsub_node_id(), [{binary(), binary()}]) ->
                         exml:element().
subscribe(User, Id, {NodeAddr, NodeName}, ConfigFields) ->
    Elements = [subscribe_element(NodeName, User) | subscribe_options_form(ConfigFields)],
    pubsub_iq(<<"set">>, User, Id, NodeAddr, Elements).

-spec unsubscribe(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
unsubscribe(User, Id, {NodeAddr, NodeName}) ->
    Elements = [unsubscribe_element(NodeName, User)],
    pubsub_iq(<<"set">>, User, Id, NodeAddr, Elements).

-spec submit_subscription_response(escalus_utils:jid_spec(), binary(), pubsub_node_id(), form()) ->
    exml:element().
submit_subscription_response(User, Id, {NodeAddr, _NodeName}, Form) ->
    Fields = [ encode_form_field(F) || F <- Form ],
    XEl = escalus_stanza:x_data_form(<<"submit">>, Fields),
    Msg = #xmlel{ name = <<"message">>,
                  attrs = [{<<"to">>, NodeAddr}, {<<"id">>, Id}],
                  children = [XEl] },
    escalus_stanza:from(Msg, escalus_utils:get_jid(User)).

-spec get_pending_subscriptions(escalus_utils:jid_spec(), binary(),
                                    pubsub_node_id() | binary()) -> exml:element().
get_pending_subscriptions(User, Id, {NodeAddr, NodeName}) ->
    Fields = [ encode_form_field({<<"pubsub#node">>, NodeName}) ],
    Payload = [ escalus_stanza:x_data_form(<<"submit">>, Fields) ],
    Node = <<"http://jabber.org/protocol/pubsub#get-pending">>,
    CommandIQ = escalus_stanza:adhoc_request(Node, Payload),
    escalus_stanza:from(escalus_stanza:set_id(escalus_stanza:to(CommandIQ, NodeAddr), Id), User);
get_pending_subscriptions(User, Id, NodesAddr) ->
    Node = <<"http://jabber.org/protocol/pubsub#get-pending">>,
    CommandIQ = escalus_stanza:adhoc_request(Node, []),
    escalus_stanza:from(escalus_stanza:set_id(escalus_stanza:to(CommandIQ, NodesAddr), Id), User).

-spec get_user_subscriptions(escalus_utils:jid_spec(), binary(),
                                  pubsub_node_id() | binary()) -> exml:element().
get_user_subscriptions(User, Id, Node) ->
    {Element, NodeAddr}
    = case Node of
          {NodeAddr0, NodeName} -> {subscriptions_element(NodeName, []), NodeAddr0};
          NodeAddr0 -> {subscriptions_element(), NodeAddr0}
      end,
    pubsub_iq(<<"get">>, User, Id, NodeAddr, [Element]).

get_subscription_options(User, Id, {NodeAddr, NodeName}) ->
    Element = subscription_options(NodeName, User),
    pubsub_iq(<<"get">>, User, Id, NodeAddr, [Element]).

set_subscription_options(User, Id, {NodeAddr, NodeName}, Options) ->
    FormType = form_type_field_element(<<"subscribe_options">>),
    EncodedOptions = [FormType | lists:map(fun encode_form_field/1, Options)],
    Children = [set_subscription_options_form(NodeName, User, EncodedOptions)],
    pubsub_iq(<<"set">>, User, Id, NodeAddr, Children).

-spec get_node_subscriptions(escalus_utils:jid_spec(), binary(), pubsub_node_id()) ->
                                         exml:element().
get_node_subscriptions(User, Id, {NodeAddr, NodeName}) ->
    Elements = [subscriptions_element(NodeName, [])],
    pubsub_owner_iq(<<"get">>, User, Id, NodeAddr, Elements).

-spec set_subscriptions(escalus_utils:jid_spec(), binary(),
                        [{escalus_utils:jid_spec(), binary()}], pubsub_node_id()) ->
                               exml:element().
set_subscriptions(User, Id, Subscriptions, {NodeAddr, NodeName}) ->
    SubElements = [subscription_element(Jid, SubState) || {Jid, SubState} <- Subscriptions],
    Elements = [subscriptions_element(NodeName, SubElements)],
    pubsub_owner_iq(<<"set">>, User, Id, NodeAddr, Elements).

%% ---------------- publish & items management ----------------

-spec publish(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
publish(User, Id, {NodeAddr, NodeName}) ->
    Elements = [publish_element(NodeName, undefined)],
    pubsub_iq(<<"set">>, User, Id, NodeAddr, Elements).

-spec publish(escalus_utils:jid_spec(), exml:element(), binary(), pubsub_node_id()) ->
                     exml:element().
publish(User, ContentElement, Id, {NodeAddr, NodeName}) ->
    ItemElement = item_element(ContentElement),
    Elements = [publish_element(NodeName, ItemElement)],
    pubsub_iq(<<"set">>, User, Id, NodeAddr, Elements).

-spec publish(escalus_utils:jid_spec(), binary(), exml:element(), binary(), pubsub_node_id()) ->
                     exml:element().
publish(User, ItemId, ContentElement, Id, {NodeAddr, NodeName}) ->
    ItemElement = item_element(ItemId, ContentElement),
    Elements = [publish_element(NodeName, ItemElement)],
    pubsub_iq(<<"set">>, User, Id, NodeAddr, Elements).

-spec publish_with_options(escalus_utils:jid_spec(),
                           binary(),
                           pubsub_node_id(),
                           publish_options()) -> exml:element().
publish_with_options(User, Id, {NodeAddr, NodeName}, PublishOptions) ->
    PublishOptionsElement = publish_options_element(PublishOptions),
    Elements = [publish_element(NodeName, undefined), PublishOptionsElement],
    pubsub_iq(<<"set">>, User, Id, NodeAddr, Elements).

-spec publish_with_options(escalus_utils:jid_spec(),
                           exml:element(),
                           binary(),
                           pubsub_node_id(),
                           publish_options()) -> exml:element().
publish_with_options(User, ContentElement, Id, {NodeAddr, NodeName}, PublishOptions) ->
    ItemElement = item_element(ContentElement),
    PublishOptionsElement = publish_options_element(PublishOptions),
    Elements = [publish_element(NodeName, ItemElement), PublishOptionsElement],
    pubsub_iq(<<"set">>, User, Id, NodeAddr, Elements).

-spec publish_with_options(escalus_utils:jid_spec(),
                           binary(),
                           exml:element(),
                           binary(),
                           pubsub_node_id(),
                           publish_options()) -> exml:element().
publish_with_options(User, ItemId, ContentElement, Id, {NodeAddr, NodeName}, PublishOptions) ->
    ItemElement = item_element(ItemId, ContentElement),
    PublishOptionsElement = publish_options_element(PublishOptions),
    Elements = [publish_element(NodeName, ItemElement), PublishOptionsElement],
    pubsub_iq(<<"set">>, User, Id, NodeAddr, Elements).

-spec retract(escalus_utils:jid_spec(), binary(), pubsub_node_id(), binary()) -> exml:element().
retract(User, Id, {NodeAddr, NodeName}, ItemId) ->
    Elements = [retract_item(NodeName, ItemId)],
    pubsub_iq(<<"set">>, User, Id, NodeAddr, Elements).

-spec get_items(escalus_utils:jid_spec(), binary(), pubsub_node_id(), pos_integer()) -> exml:element().
get_items(User, Id, {NodeAddr, NodeName}, MaxItems) when is_integer(MaxItems), MaxItems > 0 ->
    Elements = [items_element(NodeName, MaxItems)],
    pubsub_iq(<<"get">>, User, Id, NodeAddr, Elements).

-spec get_all_items(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
get_all_items(User, Id, {NodeAddr, NodeName}) ->
    Elements = [items_element(NodeName)],
    pubsub_iq(<<"get">>, User, Id, NodeAddr, Elements).

-spec get_item(escalus_utils:jid_spec(), binary(), binary(), pubsub_node_id()) -> exml:element().
get_item(User, Id, ItemId, {NodeAddr, NodeName}) ->
    BareItemsEl = items_element(NodeName),
    Item = item_element(ItemId, undefined),
    Elements = [BareItemsEl#xmlel{children = [Item]}],
    pubsub_iq(<<"get">>, User, Id, NodeAddr, Elements).

-spec purge_all_items(escalus_utils:jid_spec(), binary(), pubsub_node_id()) -> exml:element().
purge_all_items(User, Id, {NodeAddr, NodeName}) ->
    Elements = [purge_element(NodeName)],
    pubsub_owner_iq(<<"set">>, User, Id, NodeAddr, Elements).

%%-----------------------------------------------------------------------------
%% XML element construction
%%-----------------------------------------------------------------------------

%% Whole stanzas

iq(Type, From, Id, pep, Elements) ->
    Stanza = escalus_stanza:iq(Type, Elements),
    StanzaWithId = escalus_stanza:set_id(Stanza, Id),
    escalus_stanza:from(StanzaWithId, escalus_utils:get_jid(From));
iq(Type, From, Id, To, Elements) ->
    Stanza = escalus_stanza:iq(To, Type, Elements),
    StanzaWithId = escalus_stanza:set_id(Stanza, Id),
    escalus_stanza:from(StanzaWithId, escalus_utils:get_jid(From)).

pubsub_iq(Type, User, Id, NodeAddr, Elements) ->
    pubsub_iq(Type, User, Id, NodeAddr, Elements, ?NS_PUBSUB).

pubsub_iq(Type, User, Id, NodeAddr, Elements, NS) ->
    PubSubElement = pubsub_element(Elements, NS),
    iq(Type, User, Id, NodeAddr, [PubSubElement]).

pubsub_owner_iq(Type, User, Id, NodeAddr, Elements) ->
    pubsub_iq(Type, User, Id, NodeAddr, Elements, ?NS_PUBSUB_OWNER).


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

publish_options_element(Fields) ->
    #xmlel{name = <<"publish-options">>,
           children = x_data_form(<<"submit">>, form_type_field() ++ x_data_fields(Fields))}.

x_data_form(Type, Children) ->
    [escalus_stanza:x_data_form(Type, Children)].

x_data_fields(Fields) ->
    escalus_stanza:search_fields(Fields).

form_type_field() ->
    [#xmlel{name = <<"field">>,
            attrs = [{<<"var">>, <<"FORM_TYPE">>}, {<<"type">>, <<"hidden">>}],
            children = [#xmlel{name = <<"value">>,
                               children = [{xmlcdata, ?NS_PUBSUB_PUBLISH_OPTIONS}]}]}].

items_element(NodeName) ->
    #xmlel{name = <<"items">>,
           attrs = [{<<"node">>, NodeName}]}.

items_element(NodeName, MaxItems) ->
    #xmlel{name = <<"items">>,
           attrs = [{<<"node">>, NodeName},
                    {<<"max_items">>, integer_to_binary(MaxItems)}]}.

item_element(ContentElement) ->
    #xmlel{name = <<"item">>,
           children = skip_undefined([ContentElement])}.

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

subscription_options(NodeName, User) ->
    #xmlel{name = <<"options">>,
           attrs = [{<<"node">>, NodeName},
                    {<<"jid">>, escalus_utils:get_jid(User)}]}.

set_subscription_options_form(NodeName, User, Children) ->
    #xmlel{name = <<"options">>,
           attrs = [{<<"node">>, NodeName},
                    {<<"jid">>, escalus_utils:get_jid(User)}],
           children = [#xmlel{name = <<"x">>,
                              attrs = [{<<"xmlns">>, <<"jabber:x:data">>},
                                       {<<"type">>, <<"submit">>}],
                              children = Children}]}.

default_element() ->
    #xmlel{name = <<"default">>}.

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

encode_form_field({Var, Value}) ->
    encode_form_field(Var, [Value]);
encode_form_field({Var, <<"text-multi">>, Value}) ->
    encode_form_field(Var, Value);
encode_form_field({Var, _Type, Value}) ->
    encode_form_field(Var, [Value]).

encode_form_field(Var, Values) ->
    Children = [ #xmlel{name = <<"value">>, children = [#xmlcdata{content = Content}]}
                 || Content <- Values ],
    #xmlel{name = <<"field">>, attrs = [{<<"var">>, Var}], children = Children}.

%% Helpers

skip_undefined(L) ->
    lists:filter(fun(undefined) -> false;
                    ({_, undefined}) -> false;
                    (_) -> true
                 end, L).
