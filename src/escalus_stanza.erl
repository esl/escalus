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
         iq_result/1,
         presence/1,
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
         privacy_no_default/1]).

-include("include/escalus.hrl").
-include_lib("exmpp/include/exmpp.hrl").

chat_to(#client{jid=Jid}, Msg) ->
    Chat = exmpp_message:chat(Msg),
    exmpp_stanza:set_recipient(Chat, Jid).

iq_result(Request) ->
    exmpp_iq:result(Request).

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
    exmpp_presence:proble().

presence_direct(#client{jid_short=Jid}, Type) ->
    Presence = presence(Type),
    exmpp_stanza:set_recipient(Presence, Jid).

presence_show(Presence, Type) ->
    exmpp_presence:set_show(Presence, Type).

presence_status(Presence, Status) ->
    exmpp_presence:set_status(Presence, Status).

presence_priority(Presence, Priority) ->
    exmpp_presence:set_priority(Presence, Priority).

roster_get() ->
    exmpp_client_roster:get_roster().

roster_add_contact(#client{jid_short=Jid}, Group, Nick) ->
    exmpp_client_roster:set_item(Jid, Group, Nick).

roster_remove_contact(#client{jid_short=Jid}) ->
    Stanza = exmpp_client_roster:set_item(Jid, [], []),
    Query = exmpp_xml:get_element(Stanza,"query"),
    [Item] = exmpp_xml:get_child_elements(Query),
    ItemNew = exmpp_xml:set_attribute(Item, {<<"subscription">>, "remove"}),
    QueryNew = exmpp_xml:replace_child(Query, Item, ItemNew),
    exmpp_xml:replace_child(Stanza, Query, QueryNew).

privacy_get_all(#client{jid=Jid}) ->
    Query = exmpp_xml:element(?NS_PRIVACY, 'query'),
    Iq = exmpp_iq:get(?NS_JABBER_CLIENT, Query),
    exmpp_stanza:set_sender(Iq, Jid).

privacy_get_one(Client, ListName) ->
    privacy_get_many(Client, [ListName]).

privacy_get_many(#client{jid=Jid}, ListNames) ->
    Query = exmpp_xml:append_children(
        exmpp_xml:element(?NS_PRIVACY, 'query'),
        [ exmpp_xml:set_attribute(exmpp_xml:element('list'),
            {<<"name">>, ListName}) || ListName <- ListNames ]
        ),
    Iq = exmpp_iq:get(?NS_JABBER_CLIENT, Query),
    exmpp_stanza:set_sender(Iq, Jid).

privacy_set_one(#client{jid=Jid}, PrivacyList) ->
    Query = exmpp_xml:append_child(
        exmpp_xml:element(?NS_PRIVACY, 'query'), PrivacyList),
    Iq = exmpp_iq:set(?NS_JABBER_CLIENT, Query),
    exmpp_stanza:set_sender(Iq, Jid).

privacy_active_or_default(#client{jid=Jid}, What, ListName)
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
        Jid).

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
