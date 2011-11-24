%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(escalus_stanza).

-export([chat_to/2,
         chat_to_short_jid/2,
         groupchat_to/2,
         iq_result/1,
         iq_get/2,
         iq_set/2,
         presence/1,
         presence_error/2,
         presence_direct/2,
         presence_show/2,
         presence_status/2,
         presence_priority/2,
         roster_get/0,
         roster_add_contact/3,
         roster_remove_contact/1,
         privacy_get_all/1,
         privacy_get_one/2,
         privacy_get_many/2,
         privacy_set_one/2,
         privacy_activate/2,
         privacy_deactivate/1,
         privacy_default/2,
         privacy_no_default/1,
         privacy_list/2,
         privacy_list_item/1,
         last_activity/1]).

-include("include/escalus.hrl").
-include_lib("exmpp/include/exmpp.hrl").

chat_to(Recipient, Msg) ->
    Chat = exmpp_message:chat(Msg),
    exmpp_stanza:set_recipient(Chat, escalus_utils:get_jid(Recipient)).

groupchat_to(Recipient, Msg) ->
    Chat = chat_to(Recipient, Msg),
    exmpp_xml:set_attribute(Chat, {<<"type">>, <<"groupchat">>}).

chat_to_short_jid(Recipient, Msg) ->
    Chat = exmpp_message:chat(Msg),
    exmpp_stanza:set_recipient(Chat, escalus_client:short_jid(Recipient)).

iq_result(Request) ->
    exmpp_iq:result(Request).

iq_get(NS, Payload) ->
    iq_with_type(<<"get">>, NS, Payload).

iq_set(NS, Payload) ->
    iq_with_type(<<"set">>, NS, Payload).

iq_with_type(Type, NS, Payload) ->
    #xmlel{
        name = 'iq',
        attrs = [#xmlattr{name = <<"type">>,
                          value = Type}],
        children = [
            #xmlel{
                name = 'query',
                ns = NS,
                children = [Payload]
            }
    ]}.

presence(available) ->
    exmpp_presence:available();
presence(unavailable) ->
    exmpp_presence:unavailable();
presence(subscribe) ->
    exmpp_presence:subscribe();
presence(subscribed) ->
    exmpp_presence:subscribed();
presence(unsubscribe) ->
    exmpp_presence:unsubscribe();
presence(unsubscribed) ->
    exmpp_presence:unsubscribed();
presence(probe) ->
    exmpp_presence:probe().

presence_error(Presence, Error) ->
    exmpp_presence:error(Presence, Error).

%% FIXME: see roster_add_contact/3 comment
presence_direct(Recipient, Type) ->
    Presence = presence(Type),
    exmpp_stanza:set_recipient(Presence, get_short_jid(Recipient)).

presence_show(Presence, Type) ->
    exmpp_presence:set_show(Presence, Type).

presence_status(Presence, Status) ->
    exmpp_presence:set_status(Presence, Status).

presence_priority(Presence, Priority) ->
    exmpp_presence:set_priority(Presence, Priority).

roster_get() ->
    exmpp_client_roster:get_roster().

%% FIXME: there is a legacy issue here. This function should
%% use get_jid function to let the caller make decision
%% wether to use bare or full jid.
roster_add_contact(Recipient, Group, Nick) ->
    exmpp_client_roster:set_item(get_short_jid(Recipient), Group, Nick).

%% FIXME: see roster_add_contact/3 comment
roster_remove_contact(Recipient) ->
    Stanza = exmpp_client_roster:set_item(get_short_jid(Recipient), [], []),
    Query = exmpp_xml:get_element(Stanza,"query"),
    [Item] = exmpp_xml:get_child_elements(Query),
    ItemNew = exmpp_xml:set_attribute(Item, {<<"subscription">>, "remove"}),
    QueryNew = exmpp_xml:replace_child(Query, Item, ItemNew),
    exmpp_xml:replace_child(Stanza, Query, QueryNew).

privacy_get_all(Recipient) ->
    Query = exmpp_xml:element(?NS_PRIVACY, 'query'),
    Iq = exmpp_iq:get(?NS_JABBER_CLIENT, Query),
    exmpp_stanza:set_sender(Iq, escalus_utils:get_jid(Recipient)).

privacy_get_one(Client, ListName) ->
    privacy_get_many(Client, [ListName]).

privacy_get_many(Recipient, ListNames) ->
    Query = exmpp_xml:append_children(
        exmpp_xml:element(?NS_PRIVACY, 'query'),
        [ exmpp_xml:set_attribute(exmpp_xml:element('list'),
            {<<"name">>, ListName}) || ListName <- ListNames ]
        ),
    Iq = exmpp_iq:get(?NS_JABBER_CLIENT, Query),
    exmpp_stanza:set_sender(Iq, escalus_utils:get_jid(Recipient)).

privacy_set_one(Recipient, PrivacyList) ->
    Query = exmpp_xml:append_child(
        exmpp_xml:element(?NS_PRIVACY, 'query'), PrivacyList),
    Iq = exmpp_iq:set(?NS_JABBER_CLIENT, Query),
    exmpp_stanza:set_sender(Iq, escalus_utils:get_jid(Recipient)).

privacy_active_or_default(Recipient, What, ListName)
    when What =:= 'active';
         What =:= 'default' ->
    exmpp_stanza:set_sender(
        exmpp_iq:set(?NS_JABBER_CLIENT,
            exmpp_xml:append_child(
                exmpp_xml:element(?NS_PRIVACY, 'query'),
                case ListName of
                    'this-must-not-happen-by-accident' ->
                        exmpp_xml:remove_attribute(
                            exmpp_xml:element(What),
                            <<"xmlns">>);
                    _ ->
                        exmpp_xml:set_attribute(
                            exmpp_xml:remove_attribute(
                                exmpp_xml:element(What),
                                <<"xmlns">>),
                            {<<"name">>, ListName})
                end )),
        escalus_utils:get_jid(Recipient)).

privacy_activate(Client, ListName) ->
    privacy_active_or_default(Client, active, ListName).

privacy_deactivate(Client) ->
    privacy_active_or_default(Client, active,
        'this-must-not-happen-by-accident').

privacy_default(Client, ListName) ->
    privacy_active_or_default(Client, default, ListName).

privacy_no_default(Client) ->
    privacy_active_or_default(Client, default,
        'this-must-not-happen-by-accident').

%% Create empty list element with given name.
privacy_list(Name, Items) ->
    exmpp_xml:append_children(
        exmpp_xml:set_attribute(
            exmpp_xml:remove_attribute(
                exmpp_xml:element('list'),
                <<"xmlns">>),
            {<<"name">>, Name}),
        Items).

%% Create a privacy list item element, wrapping up arguments as attributes.
privacy_list_item(ItemDescription) ->
    Attrs = case ItemDescription of
        {Action, Order, Contents} ->
            [{<<"action">>, Action}, {<<"order">>, Order}];
        {Type, Value, Action, Order, Contents} ->
            [{<<"type">>, Type}, {<<"value">>, Value}, {<<"action">>, Action},
             {<<"order">>, Order}]
    end,
    ContentElements = [ exmpp_xml:element(Content) || Content <- Contents ],
    exmpp_xml:append_children(
        exmpp_xml:set_attributes(exmpp_xml:element('item'), Attrs),
        ContentElements).

last_activity(Recipient) ->
    Query = #xmlel{ns = ?NS_LAST_ACTIVITY, name = 'query'},
    Iq = exmpp_xml:set_attributes(
           #xmlel{ns = ?NS_JABBER_CLIENT, name = 'iq'},
           [{<<"type">>, "get"},
            {<<"to">>, get_short_jid(Recipient)},
            {<<"id">>, "last-" ++ integer_to_list(random:uniform(65536 * 65536))}]),
    exmpp_xml:append_child(Iq, Query).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
%% FIXME: see roster_add_contact/3 comment,
%% delete fixing that issue afterwards
get_short_jid(#client{}=Recipient) ->
    escalus_client:short_jid(Recipient);
get_short_jid(Username) when is_atom(Username) ->
    escalus_users:get_jid(Username);
get_short_jid(Jid) when is_list(Jid); is_binary(Jid) ->
    Jid.
