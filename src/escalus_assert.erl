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

-module(escalus_assert).

-export([is_chat_message/2, 
         has_no_stanzas/1, 
         is_iq/2,
         is_presence_stanza/1,
         is_presence_type/2,
         is_presence_with_show/2,
         is_presence_with_status/2,
         is_presence_with_priority/2,
         is_stanza_from/2,
         is_roster_result/1,
         is_roster_result_set/1,
         is_result/1,
         count_roster_items/2,
         roster_contains/2,
         is_privacy_query_result/1,
         is_privacy_query_result_with_active/1,
         is_privacy_query_result_with_active/2,
         is_privacy_query_result_with_default/1,
         is_privacy_query_result_with_default/2,
         is_privacy_list_nonexistent_error/1,
         is_error/3]).

-include_lib("deps/exmpp/include/exmpp.hrl").
-include("include/escalus.hrl").

%%---------------------------------------------------------------------
%% API functions
%%---------------------------------------------------------------------

is_chat_message(Msg, Stanza) when is_list(Msg) ->
    is_chat_message(list_to_binary(Msg), Stanza);
is_chat_message(Msg, Stanza) when is_binary(Msg) ->
    chat = exmpp_message:get_type(Stanza),
    Msg = exmpp_message:get_body(Stanza).

is_iq(Type, Stanza) ->
    Type = exmpp_iq:get_type(Stanza).

has_no_stanzas(Client) ->
    false = escalus_client:has_stanzas(Client).

is_presence_stanza(Stanza) ->
    "presence" = exmpp_xml:get_name_as_list(Stanza).

is_presence_type(Type, Presence) ->
    case Type of
        "available" ->
            none = exmpp_xml:get_attribute_as_list(Presence, <<"type">>, none);
        _ ->
            Type = exmpp_xml:get_attribute_as_list(Presence, <<"type">>, none)
    end.

is_presence_with_show(Show, Presence) ->
    Show = exmpp_xml:get_path(Presence, [{element, "show"},
                                       cdata_as_list]).

is_presence_with_status(Status, Presence) ->
    Status = exmpp_xml:get_path(Presence, [{element, "status"},
                                       cdata_as_list]).

is_presence_with_priority(Priority, Presence) ->
    Priority = exmpp_xml:get_path(Presence, [{element, "priority"},
                                       cdata_as_list]).
                
is_stanza_from(#client{jid=Jid}, Stanza) ->
    ExpectedJid = binary_to_list(Jid),
    ActualJid = exmpp_xml:get_attribute_as_list(Stanza, <<"from">>, none),
    true = lists:prefix(ExpectedJid, ActualJid).


is_roster_result(Stanza) ->
    "result" = exmpp_xml:get_attribute_as_list(Stanza, <<"type">>, none),
    Query = exmpp_xml:get_element(Stanza, "jabber:iq:roster", "query"),
    exmpp_xml:element_matches(Query, "query").

is_roster_result_set(Stanza) ->
    "set" = exmpp_xml:get_attribute_as_list(Stanza, <<"type">>, none).

is_result(Stanza) ->
    "result" = exmpp_xml:get_attribute_as_list(Stanza, <<"type">>, none).

count_roster_items(Num, Stanza) ->
    Items =  exmpp_xml:get_child_elements(
               exmpp_xml:get_element(Stanza, "jabber:iq:roster", "query")),
    Num = length(Items).

roster_contains(#client{jid=Jid}, Stanza) ->
    ExpectedJid = binary_to_list(Jid),
    Items =  exmpp_xml:get_child_elements(exmpp_xml:get_element(Stanza,"query")),
    true = lists:foldl(fun(Item, Res) ->
                               ContactJid = exmpp_xml:get_attribute_as_list(Item, 
                                                                        <<"jid">>, 
                                                                        none),
                               % ExpectedJid may contain the resource
                               case lists:prefix(ContactJid, ExpectedJid) of
                                   true ->
                                       true;
                                   _ ->
                                       Res
                               end
                       end, false, Items).

is_privacy_query_result(Stanza) ->
    true = exmpp_iq:is_result(Stanza),
    true = exmpp_xml:has_element(Stanza, ?NS_PRIVACY, 'query').

is_privacy_query_result_with_active(Stanza) ->
    is_privacy_query_result_with(Stanza, active, none).

is_privacy_query_result_with_default(Stanza) ->
    is_privacy_query_result_with(Stanza, default, none).

is_privacy_query_result_with_active(Stanza, ActiveListName) ->
    is_privacy_query_result_with(Stanza, active, ActiveListName).

is_privacy_query_result_with_default(Stanza, DefaultListName) ->
    is_privacy_query_result_with(Stanza, default, DefaultListName).

is_privacy_query_result_with(Stanza, What, ListName) ->
    is_privacy_query_result(Stanza),
    Query = exmpp_iq:get_result(Stanza),
    has_query_got(Query, What, ListName).

has_query_got(Query, What, ListName)
    when What =:= 'active';
         What =:= 'default' ->
    true = exmpp_xml:has_element(Query, What),
    TheElement = exmpp_xml:get_element(Query, What),
    true = exmpp_xml:has_attribute(TheElement, <<"name">>),
    true = if ListName =/= none -> atom_to_list(ListName)
               =:= exmpp_xml:get_attribute_as_list(
                   TheElement, <<"name">>, none)
            ; true -> true
           end.

is_privacy_list_nonexistent_error(Stanza) ->
    escalus_assert:is_iq('error', Stanza),

    Query = exmpp_xml:get_element(Stanza, ?NS_PRIVACY, 'query'),
    exmpp_xml:element_matches(Query, 'query'),
    List = exmpp_xml:get_element(Query, 'list'),
    exmpp_xml:element_matches(List, 'list'),
    exmpp_xml:has_attribute(List, <<"name">>),

    Error = exmpp_xml:get_element(Stanza, 'error'),
    exmpp_xml:element_matches(Error, 'error'),
    exmpp_xml:has_element(Error, ?NS_STANZA_ERRORS, 'item-not-found').

%% @spec (Stanza, Type, Condition) -> bool()
%%     Stanza = exmpp_xml:xmlel() | iq()
%%     Type = binary()
%%     Condition = atom()
is_error(Stanza, Type, Condition) ->
    try begin
        true = exmpp_iq:is_error(Stanza),
        Type = exmpp_stanza:get_error_type(Stanza),
        Condition = exmpp_stanza:get_condition(Stanza)
    end of
        _ -> true
    catch
        _:_ -> false
    end.
