%%%===================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd.
%%% @doc Suite for testing pubsub features as described in XEP-0060
%%% @Helper module - only pubsub specific stanzas generation
%%% @functions.
%%% @end
%%%===================================================================

-module(escalus_pubsub_stanza).


-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").

-export([
         create_node_stanza/4,
         create_node_stanza/5,
         configure_node_stanza/5,
         create_specific_node_stanza/1,
         create_subscribe_node_stanza/2,
         create_request_allitems_stanza/1,
         create_request_allitems_stanza_with_iq/4,
         create_publish_node_content_stanza/2,
         create_publish_node_content_stanza_second/2,
         create_publish_node_content_stanza_third/2,
         create_publish_node_content_stanza_with_timestamp/2,
         create_sub_unsubscribe_from_node_stanza/3,
         create_unsubscribe_from_node_stanza/2,
         delete_node_stanza/1,
         entry_body_sample1/0,
         entry_body_with_sample_device_id/0,
         get_subscription_change_list_stanza/1,
         iq_with_id/5,
         iq_set_get_rest/3,
         publish_item/2,
         publish_item_stanza/2,
         publish_entry/1,
         pubsub_stanza/2,
         publish_node_with_content_stanza/2,
         publish_sample_content_stanza/5,
         retract_from_node_stanza/2,
         retrieve_subscriptions_stanza/1,
         retrieve_user_subscriptions_stanza/0,
         set_subscriptions_stanza/2,
         subscribe_by_user_stanza/4,
         subscribe_by_user_stanza/5,
         unsubscribe_by_user_stanza/4,
         discover_nodes_stanza/3,
         discover_nodes_stanza/4
        ]).


pubsub_stanza(Children, NS) ->
    #xmlel{name = <<"pubsub">>,
           attrs = [{<<"xmlns">>, NS} ],
           children = Children  }.



publish_sample_content_stanza(DestinationTopicName, DestinationNode, PublishItemId, User, SampleNumber) ->
    PublishToNode = case SampleNumber of
                        sample_one ->
                            create_publish_node_content_stanza(DestinationTopicName, PublishItemId);
                        sample_two ->
                            create_publish_node_content_stanza_second(DestinationTopicName, PublishItemId);
                        sample_three ->
                            create_publish_node_content_stanza_third(DestinationTopicName, PublishItemId);
                        sample_time ->
                            create_publish_node_content_stanza_with_timestamp(DestinationTopicName, PublishItemId);
                        _ ->
                            create_publish_node_content_stanza(DestinationTopicName, PublishItemId)
                    end,
    IqId = <<"publish1">>,
    escalus_pubsub_stanza:iq_with_id(set, IqId, DestinationNode, User,  [PublishToNode]).

create_node_stanza(User, IqId, DestinationNodeAddr, DestinationNodeName) ->
    create_node_stanza(User, IqId, DestinationNodeAddr, DestinationNodeName, []).

create_node_stanza(User, IqId, DestinationNodeAddr, DestinationNodeName, Config) ->
    PubSubCreate = create_specific_node_stanza(DestinationNodeName),
    Elements = [PubSubCreate | case Config of
                                   [] -> [];
                                   _ -> [configure_node_form(Config)]
                               end],
    PubSub = pubsub_stanza(Elements, ?NS_PUBSUB),
    iq_with_id(set, IqId, DestinationNodeAddr, User,  [PubSub]).

create_specific_node_stanza(NodeName) ->
    #xmlel{name = <<"create">>, attrs = [{<<"node">>, NodeName}] }.

configure_node_stanza(User, IqId, NodeAddr, NodeName, Config) ->
    Form = configure_node_form(Config),
    ConfigureElem = Form#xmlel{attrs = [{<<"node">>, NodeName}]},
    PubSubElem = pubsub_stanza([ConfigureElem], ?NS_PUBSUB),
    iq_with_id(set, IqId, NodeAddr, User, [PubSubElem]).

configure_node_form(Fields) ->
    form(<<"configure">>, <<"node_config">>, Fields).

iq_with_id(TypeAtom, Id, To, From, Body) ->
    S1 = escalus_stanza:iq(To, atom_to_binary(TypeAtom, latin1), Body),
    iq_set_get_rest(S1, Id, From).

iq_set_get_rest(SrcIq, Id, From) ->
    S2 = escalus_stanza:set_id(SrcIq, Id),
    escalus_stanza:from(S2, escalus_utils:get_jid(From)).

%% ----------------------------- sample entry bodies ------------------------

entry_body_sample1() ->
    [
     #xmlel{name = <<"title">>, children = [#xmlcdata{content = <<"The title of content.">>}]},
     #xmlel{name = <<"summary">>, children = [#xmlcdata{content = <<"To be or not to be...">>}]}
    ].

entry_body_with_timestamp() ->
    MicroSec = usec:from_now(os:timestamp()),
    [
     #xmlel{name = <<"MSG_SENT_AT">>, children = [#xmlcdata{content = integer_to_binary(MicroSec) }]}
    ].

entry_body_with_sample_device_id() ->
    [
     #xmlel{name = <<"DEVICE_ID_SMPL0">>, children = [#xmlcdata{content = <<"2F:AB:28:FF">>}]}
    ].

entry_body_with_sample_device_id_2() ->
    [
     #xmlel{name = <<"DEVICE_ID_SMPL2">>, children = [#xmlcdata{content = <<"AA:92:1C:92">>}]}
    ].

%% ------end-------------------- sample entry bodies ------------------------

%% provide EntryBody as list of anything compliant with exml entity records.

publish_entry_children([]) ->
    entry_body_sample1();

publish_entry_children([#xmlel{}] = EntryBody) ->
    EntryBody;

publish_entry_children([#xmlel{} = Head | Tail]) ->
    [Head |  publish_entry_children(Tail)].

publish_entry(EntryBody) ->
    #xmlel{
       name = <<"entry">>,
       attrs = [{<<"xmlns">>, <<"http://www.w3.org/2005/Atom">>}],
       children = publish_entry_children(EntryBody)
      }.

publish_item_children(#xmlel{} = ItemBody) ->
    [ItemBody];
publish_item_children([]) ->
    [].

publish_item_stanza(NodeName, ItemToPublish) ->
    PublNode = publish_node_with_content_stanza(NodeName, ItemToPublish),
    pubsub_stanza([PublNode], ?NS_PUBSUB).

publish_item(ItemId, PublishEntry) ->
    #xmlel{
       name = <<"item">>,
       attrs = [{<<"id">>, ItemId}],
       children = publish_item_children(PublishEntry)
      }.

publish_node_children(#xmlel{} = ItemBody) ->
    [ItemBody];
publish_node_children([]) ->
    [].

publish_node_with_content_stanza(NodeName, ItemToPublish) ->
    #xmlel{
       name = <<"publish">>,
       attrs = [{<<"node">>, NodeName}],
       children = publish_node_children(ItemToPublish)
      }.

%% Create full sample content with nested items like in example 88 of XEP-0060
%% The structure is as follows: pubsub/publish/item/entry/(title,summary)
create_publish_node_content_stanza(NodeName, ItemId) ->
    PublishEntry = publish_entry([]),
    ItemTopublish = publish_item(ItemId, PublishEntry),
    PublNode = publish_node_with_content_stanza(NodeName, ItemTopublish),
    pubsub_stanza([PublNode], ?NS_PUBSUB).

%% Similar to above but with different entry body - mimicking "real" physical
%% device with hardware "identifier" - device 1
create_publish_node_content_stanza_second(NodeName, ItemId) ->
    PublishEntry = publish_entry(entry_body_with_sample_device_id()),
    ItemTopublish = publish_item(ItemId, PublishEntry),
    PublNode = publish_node_with_content_stanza(NodeName, ItemTopublish),
    pubsub_stanza([PublNode], ?NS_PUBSUB).

%% Similar to above but with different entry body - mimicking "real" physical
%% device with hardware "identifier" - device 2
create_publish_node_content_stanza_third(NodeName, ItemId) ->
    PublishEntry = publish_entry(entry_body_with_sample_device_id_2()),
    ItemTopublish = publish_item(ItemId, PublishEntry),
    PublNode = publish_node_with_content_stanza(NodeName, ItemTopublish),
    pubsub_stanza([PublNode], ?NS_PUBSUB).

%% timestamp is microseconds
create_publish_node_content_stanza_with_timestamp(NodeName, ItemId) ->
    PublishEntry = publish_entry(entry_body_with_timestamp()),
    ItemTopublish = publish_item(ItemId, PublishEntry),
    PublNode = publish_node_with_content_stanza(NodeName, ItemTopublish),
    pubsub_stanza([PublNode], ?NS_PUBSUB).


retract_from_node_stanza(NodeName, ItemId) ->
    ItemToRetract = #xmlel{name = <<"item">>, attrs=[{<<"id">>, ItemId}], children=[]},
    RetractNode =  #xmlel{name = <<"retract">>, attrs=[{<<"node">>, NodeName}], children=[ItemToRetract]},
    pubsub_stanza([RetractNode], ?NS_PUBSUB).

%% ------------ subscribe - unscubscribe -----------

subscribe_by_user_stanza(User, IqId, NodeName, NodeAddress) ->
    subscribe_by_user_stanza(User, IqId, NodeName, NodeAddress, []).

subscribe_by_user_stanza(User, IqId, NodeName, NodeAddress, Config) ->
    SubscribeToNode = create_subscribe_node_stanza(NodeName, User, Config),
    iq_with_id(set, IqId, NodeAddress, User,  [SubscribeToNode]).

unsubscribe_by_user_stanza(User, IqId, NodeName, NodeAddress) ->
    UnubscribeFromNode = create_unsubscribe_from_node_stanza(NodeName, User),
    escalus_pubsub_stanza:iq_with_id(set, IqId, NodeAddress, User,  [UnubscribeFromNode]).

create_subscribe_node_stanza(NodeName, From) ->
    create_subscribe_node_stanza(NodeName, From, []).

create_subscribe_node_stanza(NodeName, From, Config) ->
    SubscrNode = create_sub_unsubscribe_from_node_stanza(NodeName, From, <<"subscribe">>),
    Elements = [SubscrNode | case Config of
                                 [] -> [];
                                 _ -> [subscribe_options_form(Config)]
                             end],
    pubsub_stanza(Elements, ?NS_PUBSUB).

subscribe_options_form(Fields) ->
    form(<<"options">>, <<"subscribe_options">>, Fields).

create_unsubscribe_from_node_stanza(NodeName, From) ->
    UnsubsrNode = create_sub_unsubscribe_from_node_stanza(NodeName, From, <<"unsubscribe">>),
    pubsub_stanza([UnsubsrNode], ?NS_PUBSUB).

create_sub_unsubscribe_from_node_stanza(NodeName, From, SubUnsubType) ->
    #xmlel{name = SubUnsubType,
           attrs = [
                    {<<"node">>, NodeName},
                    {<<"jid">>, escalus_utils:get_jid(From)}]
          }.

%% ----end----- subscribe - unscubscribe -----------

create_request_allitems_stanza(NodeName) ->
    AllItems = #xmlel{name = <<"items">>, attrs=[{<<"node">>, NodeName}]},
    pubsub_stanza([AllItems], ?NS_PUBSUB).

%%todo: make one function with option or like with create return always with iq.
create_request_allitems_stanza_with_iq(User, IqId, PubSubAddr, NodeName) ->
    AllItems = #xmlel{name = <<"items">>, attrs=[{<<"node">>, NodeName}]},
    iq_with_id(get, IqId, PubSubAddr, User, [pubsub_stanza([AllItems], ?NS_PUBSUB)]).


delete_node_stanza(NodeName) ->
    DelNode = #xmlel{name = <<"delete">>,
                     attrs = [{<<"node">>, NodeName}]
                    },
    pubsub_stanza([DelNode], ?NS_PUBSUB_OWNER).

retrieve_subscriptions_stanza(NodeName) ->
    RetrieveNode = #xmlel{name = <<"subscriptions">>,
                          attrs = [{<<"node">>, NodeName}]
                         },
    pubsub_stanza([RetrieveNode], ?NS_PUBSUB_OWNER).

retrieve_user_subscriptions_stanza() ->
    RetrieveNode = #xmlel{name = <<"subscriptions">>},
    pubsub_stanza([RetrieveNode], ?NS_PUBSUB).

subscription_stanza({Jid, SubscriptionState}) ->
    #xmlel{name = <<"subscription">>,
           attrs = [{<<"jid">>, Jid},{<<"subscription">>, SubscriptionState}]
          }.

%% according to Example 187, 8.8.2.1, XEP-0060
set_subscriptions_stanza(NodeName, SubscriptionChangesListStanza) ->
    RetrieveNode = #xmlel{
                      name = <<"subscriptions">>,
                      attrs = [{<<"node">>, NodeName}],
                      children = SubscriptionChangesListStanza
                     },
    pubsub_stanza([RetrieveNode], ?NS_PUBSUB_OWNER).

%% pass SubscrChangeData as List of tuples {jid, new_subscription_state}
get_subscription_change_list_stanza(SubscriptionChangeData) ->
    lists:map(
      fun(ChangeEntry) ->
              subscription_stanza({_Jid, _SubsState} = ChangeEntry)
      end,
      SubscriptionChangeData).

discover_nodes_stanza(User, IqId, NodeAddr) ->
    Query = escalus_stanza:query_el(?NS_DISCO_ITEMS, [], []),
    iq_with_id(get, IqId, NodeAddr, User,  [Query]).

discover_nodes_stanza(User, IqId, NodeAddr, NodeName) ->
    Query = escalus_stanza:query_el(?NS_DISCO_ITEMS, [{<<"node">>, NodeName}], []),
    iq_with_id(get, IqId, NodeAddr, User,  [Query]).

%%-----------------------------------------------------------------------------
%% Form utils
%%-----------------------------------------------------------------------------

form(ElemName, FormType, Fields) ->
    #xmlel{name = ElemName,
           children = [#xmlel{name = <<"x">>,
                              attrs = [{<<"xmlns">>, <<"jabber:x:data">>},
                                       {<<"type">>, <<"submit">>}],
                              children = [hidden_field(FormType) | form_fields(Fields)]}
                      ]}.

hidden_field(FormType) ->
    Content = << <<"http://jabber.org/protocol/pubsub#">>/binary, FormType/binary >>,
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, <<"FORM_TYPE">>},
                    {<<"type">>, <<"hidden">>}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = Content}]}]}.

form_fields(Config) ->
    [#xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Var}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = Content}]}]}
     || {Var, Content} <- Config].
