%%%===================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd.
%%% @doc Suite for testing pubsub features as described in XEP-0060
%%% @Helper module - only pubsub specific stanzas generation
%%% @functions. 
%%% @end
%%%===================================================================

-module(escalus_pubsub_stanza).
%%-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").

-export([
	 create_node_stanza/3,
	 create_specific_node_stanza/1,
	 create_subscribe_node_stanza/2,
	 create_request_allitems_stanza/1,
	 create_publish_node_content_stanza/2,
	 create_publish_node_content_stanza_second/2,
	 create_publish_node_content_stanza_third/2,
	 create_sub_unsubscribe_from_node_stanza/3,
	 create_unsubscribe_from_node_stanza/2,
	 delete_node_stanza/1,
	 entry_body_sample1/0,
	 entry_body_with_sample_device_id/0,
	 get_subscription_change_list_stanza/1,
	 iq_with_id/5,
	 iq_set_get_rest/3,
	 publish_item/2,
	 publish_entry/1,
	 pubsub_stanza/2,
	 publish_node_with_content_stanza/2,
	 publish_sample_content_stanza/5,
	 retract_from_node_stanza/2,
	 retrieve_subscriptions_stanza/1,
	 set_subscriptions_stanza/2
]).


pubsub_stanza(Children, NS) ->
    #xmlel{name = <<"pubsub">>,
	   attrs = [{<<"xmlns">>, NS} ],
	   children = Children  }.

publish_sample_content_stanza(DestinationTopicName, DestinationNode, PublishItemId, User, SampleNumber) ->
    PublishToNode = case SampleNumber of
	sample_one -> create_publish_node_content_stanza(DestinationTopicName, PublishItemId);
	sample_two -> create_publish_node_content_stanza_second(DestinationTopicName, PublishItemId);
	sample_three -> create_publish_node_content_stanza_third(DestinationTopicName, PublishItemId);
	_ -> create_publish_node_content_stanza(DestinationTopicName, PublishItemId)
    end,
   IqId = <<"publish1">>,
   escalus_pubsub_stanza:iq_with_id(set, IqId, DestinationNode, User,  [PublishToNode]).

create_node_stanza(User, DestinationNodeAddr, DestinationNodeName) ->
   PubSubCreate = create_specific_node_stanza(DestinationNodeName),
   PubSub = pubsub_stanza([PubSubCreate], ?NS_PUBSUB),
   Id = <<"create1">>,
   iq_with_id(set, Id, DestinationNodeAddr, User,  [PubSub]).

create_specific_node_stanza(NodeName) ->
    #xmlel{name = <<"create">>,
	   attrs = [{<<"node">>, NodeName}] }.

iq_with_id(TypeAtom, Id, To, From, Body) ->
    S1 = escalus_stanza:iq(To, atom_to_binary(TypeAtom, latin1), Body),
    iq_set_get_rest(S1, Id, From).

iq_set_get_rest(SrcIq, Id, From) ->
    S2 = escalus_stanza:set_id(SrcIq, Id),							
    escalus_stanza:from(S2, escalus_utils:get_jid(From)).

%% ----------------------------- sample entry bodies ------------------------

entry_body_sample1() ->
    [
     #xmlel{name = <<"title">>, children  = [ #xmlcdata{content=[<<"The title of content.">>]}]},
     #xmlel{name = <<"summary">>, children= [ #xmlcdata{content=[<<"To be or not to be...">>]}]}
    ].

entry_body_with_sample_device_id() ->
    [
     #xmlel{name = <<"DEVICE_ID_SMPL0">>, children  = [ #xmlcdata{content=[<<"2F:AB:28:FF">>]}]}
    ].

entry_body_with_sample_device_id_2() ->
    [
     #xmlel{name = <<"DEVICE_ID_SMPL2">>, children  = [ #xmlcdata{content=[<<"AA:92:1C:92">>]}]}
    ].

%% ------end-------------------- sample entry bodies ------------------------

%% provide EntryBody as list of anything compliant with exml entity records.

publish_entry_children([]) ->
    entry_body_sample1();

publish_entry_children([#xmlel{}] = EntryBody) ->
    EntryBody;

publish_entry_children(#xmlel{} = EntryBody) ->
    [EntryBody].

publish_entry(EntryBody) ->
    #xmlel{
       name = <<"entry">>,
       attrs = [{<<"xmlns">>, <<"http://www.w3.org/2005/Atom">>}],
       children = publish_entry_children(EntryBody)
      }.

publish_item_children(#xmlel{} = ItemBody) ->
    publish_entry(ItemBody);

publish_item_children([]) ->
    publish_entry([]).
    
publish_item(ItemId, PublishEntry) ->
    #xmlel{
       name = <<"item">>,
       attrs = [{<<"id">>, ItemId}],
       children = [publish_item_children(PublishEntry)]
      }.

publish_node_children([]) ->
    publish_item(<<"abc123">>, []);

publish_node_children(#xmlel{} = ItemBody) ->
    ItemBody;

publish_node_children(_) ->
    publish_node_children([]).

publish_node_with_content_stanza(NodeName, ItemToPublish) ->
    #xmlel{
       name = <<"publish">>,
       attrs = [{<<"node">>, NodeName}],
       children = [publish_node_children(ItemToPublish)]
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

retract_from_node_stanza(NodeName, ItemId) ->
    ItemToRetract = #xmlel{name = <<"item">>, attrs=[{<<"id">>, ItemId}], children=[]},
    RetractNode =  #xmlel{name = <<"retract">>, attrs=[{<<"node">>, NodeName}], children=[ItemToRetract]},
    pubsub_stanza([RetractNode], ?NS_PUBSUB).

%% ------------ subscribe - unscubscribe -----------

create_subscribe_node_stanza(NodeName, From) ->
    SubsrNode = create_sub_unsubscribe_from_node_stanza(NodeName, From, <<"subscribe">>),
    pubsub_stanza([SubsrNode], ?NS_PUBSUB).

create_request_allitems_stanza(NodeName) ->
    AllItems = #xmlel{name = <<"items">>, attrs=[{<<"node">>, NodeName}]},
    pubsub_stanza([AllItems], ?NS_PUBSUB).

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
    lists:map(fun(ChangeEntry) -> subscription_stanza({_Jid, _SubsState} = ChangeEntry) end, SubscriptionChangeData).
