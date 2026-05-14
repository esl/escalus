%%%===================================================================
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%% @doc Stanzas for testing XEP-0060 PubSub
%%% @end
%%%===================================================================

-module(escalus_pubsub_stanza).

-include("escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-export([
         discover_nodes/2,

         create_node/2, create_node/3, delete_node/2,

         get_configuration/2, set_configuration/3,
         get_default_configuration/1,

         get_affiliations/2, set_affiliations/3,

         subscribe/3, subscribe/4, unsubscribe/3,
         submit_subscription_response/2,
         get_pending_subscriptions/2,
         get_user_subscriptions/2,
         get_subscription_options/3,
         set_subscription_options/4,
         get_node_subscriptions/2,
         set_subscriptions/3,
         pubsub_owner_iq/3,
         pubsub_iq/3,
         pubsub_iq/4,
         iq/3,

         publish/2, publish/3, publish/4, publish_raw/3,
         publish_with_options/3, publish_with_options/4, publish_with_options/5,

         retract/4,
         get_items/3,
         get_all_items/2,
         get_item/3,
         purge_all_items/2
        ]).

%% Note: Some helpers produce stanzas with missing elements/attrs when node name is undefined.
%%       This is allowed in order to facilitate testing error conditions.
-type pubsub_node_name() :: binary() | undefined.

-export_type([pubsub_node_name/0]).

-type form_field() :: {Var :: binary(), Value :: binary() | [binary()]}.

%%-----------------------------------------------------------------------------
%% Request construction
%%-----------------------------------------------------------------------------

%% ---------------- disco ----------------

-spec discover_nodes(binary(), pubsub_node_name()) -> exml:element().
discover_nodes(Id, NodeName) ->
    QueryElement = escalus_stanza:query_el(?NS_DISCO_ITEMS, node_attr(NodeName), []),
    iq(<<"get">>, Id, [QueryElement]).

%% ---------------- create & delete ----------------

-spec create_node(binary(), pubsub_node_name()) -> exml:element().
create_node(Id, Node) ->
    create_node(Id, Node, []).

-spec create_node(binary(), pubsub_node_name(), [form_field()] | undefined) -> exml:element().
create_node(Id, NodeName, ConfigFields) ->
    Elements = [create_node_element(NodeName) | configure_node_form(ConfigFields, undefined)],
    pubsub_iq(<<"set">>, Id, Elements).

-spec delete_node(binary(), pubsub_node_name()) -> exml:element().
delete_node(Id, NodeName) ->
    Elements = [delete_element(NodeName)],
    pubsub_owner_iq(<<"set">>, Id, Elements).

%% ---------------- configuration ----------------

-spec get_configuration(binary(), pubsub_node_name()) -> exml:element().
get_configuration(Id, NodeName) ->
    Elements = [#xmlel{name = <<"configure">>, attrs = node_attr(NodeName)}],
    pubsub_owner_iq(<<"get">>, Id, Elements).

-spec set_configuration(binary(), pubsub_node_name(), [form_field()] | undefined) -> exml:element().
set_configuration(Id, NodeName, ConfigFields) ->
    Elements = configure_node_form(ConfigFields, NodeName),
    pubsub_owner_iq(<<"set">>, Id, Elements).

-spec get_default_configuration(binary()) -> exml:element().
get_default_configuration(Id) ->
    Elements = [default_element()],
    pubsub_owner_iq(<<"get">>, Id, Elements).

%% ---------------- affiliations ----------------

-spec get_affiliations(binary(), pubsub_node_name()) -> exml:element().
get_affiliations(Id, NodeName) ->
    Elements = [#xmlel{name = <<"affiliations">>,
                       attrs = node_attr(NodeName)}],
    pubsub_owner_iq(<<"get">>, Id, Elements).

-spec set_affiliations(binary(), pubsub_node_name(), [{escalus_utils:jid_spec(), binary()}]) ->
    exml:element().
set_affiliations(Id, NodeName, AffChange) ->
    AffList = [ #xmlel{name = <<"affiliation">>,
                       attrs = #{<<"jid">> => get_short_jid(U),
                                 <<"affiliation">> => A}}
                || {U, A} <- AffChange ],
    Affiliations = #xmlel{name = <<"affiliations">>,
                          attrs = node_attr(NodeName),
                          children = AffList},
    pubsub_owner_iq(<<"set">>, Id, [Affiliations]).

%% ---------------- subscriptions ----------------

-spec subscribe(escalus_utils:jid_spec(), binary(), pubsub_node_name()) -> exml:element().
subscribe(User, Id, Node) ->
    subscribe(User, Id, Node, []).

-spec subscribe(escalus_utils:jid_spec(), binary(), pubsub_node_name(),
                [form_field()] | undefined) ->
          exml:element().
subscribe(User, Id, NodeName, ConfigFields) ->
    Elements = [subscribe_element(NodeName, User) | subscribe_options_form(ConfigFields)],
    pubsub_iq(<<"set">>, Id, Elements).

-spec unsubscribe(escalus_utils:jid_spec(), binary(), pubsub_node_name()) -> exml:element().
unsubscribe(User, Id, NodeName) ->
    Elements = [unsubscribe_element(NodeName, User)],
    pubsub_iq(<<"set">>, Id, Elements).

-spec submit_subscription_response(binary(), [form_field()]) -> exml:element().
submit_subscription_response(Id, Form) ->
    Fields = [ encode_form_field(F) || F <- Form ],
    XEl = escalus_stanza:x_data_form(<<"submit">>, Fields),
    #xmlel{ name = <<"message">>,
            attrs = #{<<"id">> => Id},
            children = [XEl] }.

-spec get_pending_subscriptions(binary(), pubsub_node_name()) -> exml:element().
get_pending_subscriptions(Id, NodeName) ->
    Payload = pending_subscriptions_form(NodeName),
    Node = <<"http://jabber.org/protocol/pubsub#get-pending">>,
    CommandIQ = escalus_stanza:adhoc_request(Node, Payload),
    escalus_stanza:set_id(CommandIQ, Id).

-spec get_user_subscriptions(binary(), pubsub_node_name()) -> exml:element().
get_user_subscriptions(Id, NodeName) ->
    Element = subscriptions_element(NodeName, []),
    pubsub_iq(<<"get">>, Id, [Element]).

-spec get_subscription_options(escalus_utils:jid_spec(), binary(), pubsub_node_name()) ->
    exml:element().
get_subscription_options(User, Id, NodeName) ->
    Element = subscription_options(NodeName, User),
    pubsub_iq(<<"get">>, Id, [Element]).

-spec set_subscription_options(escalus_utils:jid_spec(), binary(), pubsub_node_name(),
                               [form_field()] | undefined) -> exml:element().
set_subscription_options(User, Id, NodeName, Options) ->
    Attrs = #{<<"jid">> => escalus_utils:get_jid(User)},
    Children = form(<<"options">>, NodeName, Attrs, <<"subscribe_options">>, Options),
    pubsub_iq(<<"set">>, Id, Children).

-spec get_node_subscriptions(binary(), pubsub_node_name()) -> exml:element().
get_node_subscriptions(Id, NodeName) ->
    Elements = [subscriptions_element(NodeName, [])],
    pubsub_owner_iq(<<"get">>, Id, Elements).

-spec set_subscriptions(binary(), [{escalus_utils:jid_spec(), binary()}], pubsub_node_name()) ->
    exml:element().
set_subscriptions(Id, Subscriptions, NodeName) ->
    SubElements = [subscription_element(Jid, SubState) || {Jid, SubState} <- Subscriptions],
    Elements = [subscriptions_element(NodeName, SubElements)],
    pubsub_owner_iq(<<"set">>, Id, Elements).

%% ---------------- publish & items management ----------------

-spec publish(binary(), pubsub_node_name()) -> exml:element().
publish(Id, Node) ->
    publish_raw([], Id, Node).

-spec publish(exml:element(), binary(), pubsub_node_name()) -> exml:element().
publish(ContentElement, Id, Node) ->
    publish(undefined, ContentElement, Id, Node).

-spec publish(binary() | undefined, exml:element(), binary(), pubsub_node_name()) -> exml:element().
publish(ItemId, ContentElement, Id, Node) ->
    publish_raw([item_element(ItemId, ContentElement)], Id, Node).

-spec publish_with_options(binary(), pubsub_node_name(), [form_field()] | undefined) ->
    exml:element().
publish_with_options(Id, Node, PublishOptions) ->
    publish_raw([], publish_options_form(PublishOptions), Id, Node).

-spec publish_with_options(exml:element(), binary(), pubsub_node_name(),
                           [form_field()] | undefined) -> exml:element().
publish_with_options(ContentElement, Id, Node, PublishOptions) ->
    publish_with_options(undefined, ContentElement, Id, Node, PublishOptions).

-spec publish_with_options(binary() | undefined, exml:element(), binary(), pubsub_node_name(),
                           [form_field()] | undefined) -> exml:element().
publish_with_options(ItemId, ContentElement, Id, NodeName, PublishOptions) ->
    ItemElement = item_element(ItemId, ContentElement),
    publish_raw([ItemElement], publish_options_form(PublishOptions), Id, NodeName).

-spec publish_raw([exml:element()], binary(), pubsub_node_name()) -> exml:element().
publish_raw(Children, Id, NodeName) ->
    Elements = [publish_element(NodeName, Children)],
    pubsub_iq(<<"set">>, Id, Elements).

-spec publish_raw([exml:element()], [exml:element()], binary(), pubsub_node_name()) ->
          exml:element().
publish_raw(Children, ExtraElements, Id, NodeName) ->
    Elements = [publish_element(NodeName, Children) | ExtraElements],
    pubsub_iq(<<"set">>, Id, Elements).

-spec retract(binary(), pubsub_node_name(), binary(), exml:attrs()) ->
    exml:element().
retract(Id, NodeName, ItemId, Attrs) ->
    Elements = [retract_item(NodeName, ItemId, Attrs)],
    pubsub_iq(<<"set">>, Id, Elements).

-spec get_items(binary(), pubsub_node_name(), pos_integer()) -> exml:element().
get_items(Id, NodeName, MaxItems) when is_integer(MaxItems), MaxItems > 0 ->
    Elements = [items_element(NodeName, MaxItems)],
    pubsub_iq(<<"get">>, Id, Elements).

-spec get_all_items(binary(), pubsub_node_name()) -> exml:element().
get_all_items(Id, NodeName) ->
    Elements = [items_element(NodeName)],
    pubsub_iq(<<"get">>, Id, Elements).

-spec get_item(binary(), binary(), pubsub_node_name()) -> exml:element().
get_item(Id, ItemId, NodeName) ->
    BareItemsEl = items_element(NodeName),
    Item = item_element(ItemId, undefined),
    Elements = [BareItemsEl#xmlel{children = [Item]}],
    pubsub_iq(<<"get">>, Id, Elements).

-spec purge_all_items(binary(), pubsub_node_name()) -> exml:element().
purge_all_items(Id, NodeName) ->
    Elements = [purge_element(NodeName)],
    pubsub_owner_iq(<<"set">>, Id, Elements).

%%-----------------------------------------------------------------------------
%% XML element construction
%%-----------------------------------------------------------------------------

%% Whole stanzas

-spec pubsub_owner_iq(binary(), binary(), [exml:cdata() | exml:element()]) -> exml:element().
pubsub_owner_iq(Type, Id, Elements) ->
    pubsub_iq(Type, Id, Elements, ?NS_PUBSUB_OWNER).

-spec pubsub_iq(binary(), binary(), [exml:cdata() | exml:element()]) -> exml:element().
pubsub_iq(Type, Id, Elements) ->
    pubsub_iq(Type, Id, Elements, ?NS_PUBSUB).

-spec pubsub_iq(binary(), binary(), [exml:cdata() | exml:element()], binary()) ->
    exml:element().
pubsub_iq(Type, Id, Elements, NS) ->
    PubSubElement = pubsub_element(Elements, NS),
    iq(Type, Id, [PubSubElement]).

-spec iq(binary(), binary(), [exml:cdata() | exml:element()]) -> exml:element().
iq(Type, Id, Elements) ->
    Stanza = escalus_stanza:iq(Type, Elements),
    escalus_stanza:set_id(Stanza, Id).

%% Form utils

configure_node_form(Fields, NodeName) ->
    form(<<"configure">>, NodeName, #{}, <<"node_config">>, Fields).

subscribe_options_form(Fields) ->
    form(<<"options">>, undefined, #{}, <<"subscribe_options">>, Fields).

publish_options_form(Fields) ->
    form(<<"publish-options">>, undefined, #{}, <<"publish-options">>, Fields).

form(_, _, _, _, undefined) ->
    [];
form(FormName, NodeName, Attrs, Type, Fields) ->
    FormTypeField = form_type_field_element(Type),
    FormFields = [encode_form_field(F) || F <- Fields],
    [form_element(FormName, NodeName, Attrs, [FormTypeField | FormFields])].

-spec pending_subscriptions_form(pubsub_node_name()) -> [exml:element()].
pending_subscriptions_form(undefined) ->
    [];
pending_subscriptions_form(NodeName) ->
    Fields = [encode_form_field({<<"pubsub#node">>, NodeName})],
    [escalus_stanza:x_data_form(<<"submit">>, Fields)].

%% Elements

create_node_element(NodeName) ->
    #xmlel{name = <<"create">>, attrs = node_attr(NodeName)}.

-spec pubsub_element([exml:cdata() | exml:element()], binary()) -> exml:element().
pubsub_element(Children, NS) ->
    #xmlel{name = <<"pubsub">>,
           attrs = skip_undefined(#{<<"xmlns">> => NS}),
           children = Children}.

delete_element(NodeName) ->
    #xmlel{name = <<"delete">>,
           attrs = node_attr(NodeName)}.

subscribe_element(NodeName, User) ->
    #xmlel{name = <<"subscribe">>,
           attrs = (node_attr(NodeName))#{<<"jid">> => escalus_utils:get_jid(User)}}.

unsubscribe_element(NodeName, User) ->
    #xmlel{name = <<"unsubscribe">>,
           attrs = (node_attr(NodeName))#{<<"jid">> => escalus_utils:get_jid(User)}}.

publish_element(NodeName, Children) ->
    #xmlel{name = <<"publish">>,
           attrs = node_attr(NodeName),
           children = Children}.

items_element(NodeName) ->
    #xmlel{name = <<"items">>,
           attrs = node_attr(NodeName)}.

items_element(NodeName, MaxItems) ->
    #xmlel{name = <<"items">>,
           attrs = (node_attr(NodeName))#{<<"max_items">> => integer_to_binary(MaxItems)}}.

item_element(ItemId, ContentElement) ->
    #xmlel{name = <<"item">>,
           attrs = skip_undefined(#{<<"id">> => ItemId}),
           children = maybe_children([ContentElement])}.

retract_item(NodeName, ItemId, Attrs) ->
    #xmlel{name = <<"retract">>,
           attrs = maps:merge(Attrs, node_attr(NodeName)),
           children = [#xmlel{name = <<"item">>,
                              attrs = #{<<"id">> => ItemId} }]}.

purge_element(NodeName) ->
    #xmlel{name = <<"purge">>,
           attrs = node_attr(NodeName)}.

subscriptions_element(NodeName, Children) ->
    #xmlel{name = <<"subscriptions">>,
           attrs = node_attr(NodeName),
           children = Children}.

subscription_element(User, SubscriptionState) ->
    #xmlel{name = <<"subscription">>,
           attrs = #{<<"jid">> => escalus_utils:get_jid(User),
                     <<"subscription">> => SubscriptionState}}.

subscription_options(NodeName, User) ->
    #xmlel{name = <<"options">>,
           attrs = (node_attr(NodeName))#{<<"jid">> => escalus_utils:get_jid(User)}}.

default_element() ->
    #xmlel{name = <<"default">>}.

form_element(FormName, NodeName, Attrs, FieldElements) ->
    #xmlel{name = FormName,
           attrs = maps:merge(node_attr(NodeName), Attrs),
           children = [escalus_stanza:x_data_form(<<"submit">>, FieldElements)]}.

form_type_field_element(FormType) ->
    Content = << <<"http://jabber.org/protocol/pubsub#">>/binary, FormType/binary >>,
    escalus_stanza:field_el(<<"FORM_TYPE">>, <<"hidden">>, Content).

encode_form_field({Var, Values}) when is_list(Values) ->
    encode_form_field(Var, Values);
encode_form_field({Var, Value}) ->
    encode_form_field(Var, [Value]).

encode_form_field(Var, Values) ->
    escalus_stanza:field_el(Var, undefined, Values).

%% Helpers

maybe_children(L) ->
    lists:filter(fun(undefined) -> false;
                    ({_, undefined}) -> false;
                    (_) -> true
                 end, L).

node_attr(NodeName) ->
    skip_undefined(#{<<"node">> => NodeName}).

skip_undefined(L) ->
    maps:filter(fun(_, Val) -> undefined =/= Val end, L).

-doc "Like escalus_utils:get_short_jid/1, but accepts server JIDs".
-spec get_short_jid(escalus_utils:jid_spec()) -> binary().
get_short_jid(ClientOrJid) ->
   case is_binary(ClientOrJid) andalso binary:match(ClientOrJid, ~"/") =:= nomatch of
       true -> ClientOrJid; % already no resource
       false -> escalus_utils:get_short_jid(ClientOrJid)
   end.
