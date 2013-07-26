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

%% This module contains predicates (boolean value - returning functions)
%% That are meant to be used primarly with escalus:assert function

-module(escalus_pred).

-export([is_message/1,
         is_chat_message/1,
         is_chat_message/2,
         is_iq/1,
         is_iq/2,
         is_iq/3,
         is_iq_with_ns/2,
         is_iq_set/1,
         is_iq_get/1,
         is_iq_error/1,
         is_iq_result/1,
         is_presence/1,
         is_presence_stanza/1, % backwards compatibility
         is_presence_type/2, %% backwards compatibility
         is_presence_with_type/2,
         is_presence_with_show/2,
         is_presence_with_status/2,
         is_presence_with_priority/2,
         is_stanza_from/2,
         is_roster_get/1,
         is_roster_set/1,
         is_roster_result/1,
         is_last_result/1,
         is_private_result/1,
         is_private_error/1,
         is_result/1, %% backwards compatibility
         count_roster_items/2,
         roster_contains/2,
         is_error/3,
         is_stream_error/3,
         is_privacy_set/1,
         has_type/2,
         is_privacy_result/1,
         is_privacy_result_with_active/1,
         is_privacy_result_with_default/1,
         is_privacy_result_with_active/2,
         is_privacy_result_with_default/2,
         is_privacy_list_nonexistent_error/1,
         is_adhoc_response/3,
         has_service/2,
         has_feature/2,
         has_item/2,
         has_no_such_item/2,
         has_identity/3,
         stanza_timeout/1,
         is_stream_end/1,
         is_bosh_report/2
     ]).

-include("include/escalus.hrl").
-include("escalus_xmlns.hrl").
-include("escalus_deprecated.hrl").
-include_lib("exml/include/exml_stream.hrl").

-import(escalus_compat, [bin/1]).

%%--------------------------------------------------------------------
%% Deprecation support
%%--------------------------------------------------------------------

?DEPRECATED1(is_presence_stanza, is_presence).
?DEPRECATED2(is_presence_type, is_presence_with_type).
?DEPRECATED1(is_result, is_iq_result).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

is_presence(#xmlel{name = <<"presence">>}) ->
    true;
is_presence(_) ->
    false.

is_presence_with_type(<<"available">>, Pres) ->
    is_presence_with_type(undefined, Pres);
is_presence_with_type(Type, Pres) ->
    is_presence(Pres)
    andalso
    has_type(Type, Pres).

is_message(#xmlel{name = <<"message">>}) ->
    true;
is_message(_) ->
    false.

is_iq(#xmlel{name = <<"iq">>}) ->
    true;
is_iq(_) ->
    false.

is_iq(Type, Stanza) ->
    is_iq(Stanza)
    andalso
    has_type(Type, Stanza).

is_iq(Type, NS, Stanza) ->
    is_iq_with_ns(NS, Stanza)
    andalso
    has_type(Type, Stanza).

is_iq_with_ns(NS, Stanza) ->
    is_iq(Stanza)
    andalso
    bin(NS) == exml_query:path(Stanza, [{element, <<"query">>},
                                        {attr, <<"xmlns">>}]).

is_chat_message(Stanza) ->
    is_message(Stanza)
    andalso
    has_type(<<"chat">>, Stanza).

is_chat_message(Msg, Stanza) ->
    is_chat_message(Stanza)
    andalso
    bin(Msg) == exml_query:path(Stanza, [{element, <<"body">>}, cdata]).

has_type(undefined, Stanza) ->
    undefined == exml_query:attr(Stanza, <<"type">>);
has_type(Type, Stanza) ->
    bin(Type) == bin(exml_query:attr(Stanza, <<"type">>)).

is_iq_set(Stanza) -> is_iq(<<"set">>, Stanza).
is_iq_get(Stanza) -> is_iq(<<"get">>, Stanza).
is_iq_error(Stanza) -> is_iq(<<"error">>, Stanza).
is_iq_result(Stanza) -> is_iq(<<"result">>, Stanza).

is_presence_with_show(Show, Presence) ->
    is_presence(Presence)
    andalso
    bin(Show) == exml_query:path(Presence, [{element, <<"show">>}, cdata]).

is_presence_with_status(Status, Presence) ->
    is_presence(Presence)
    andalso
    bin(Status) == exml_query:path(Presence, [{element, <<"status">>}, cdata]).

is_presence_with_priority(Priority, Presence) ->
    is_presence(Presence)
    andalso
    bin(Priority) == exml_query:path(Presence, [{element, <<"priority">>}, cdata]).

is_stanza_from(From, Stanza) ->
    ExpectedJid = escalus_utils:get_jid(From),
    ActualJid = exml_query:attr(Stanza, <<"from">>),
    escalus_utils:is_prefix(ExpectedJid, ActualJid).

is_roster_get(Stanza) ->
    is_iq(<<"get">>, ?NS_ROSTER, Stanza).

is_roster_set(Stanza) ->
    is_iq(<<"set">>, ?NS_ROSTER, Stanza).

is_roster_result(Stanza) ->
    is_iq(<<"result">>, ?NS_ROSTER, Stanza).

is_last_result(Stanza) ->
    is_iq(<<"result">>, ?NS_LAST_ACTIVITY, Stanza).

is_private_result(Stanza) ->
    is_iq(<<"result">>, ?NS_PRIVATE, Stanza).

is_private_error(Stanza) ->
    is_iq(<<"error">>, ?NS_PRIVATE, Stanza).

roster_contains(Contact, Stanza) ->
    ExpectedJid = escalus_utils:get_jid(Contact),
    Items = get_roster_items(Stanza),
    lists:any(fun (#xmlel{} = Element) ->
                      ContactJid = exml_query:attr(Element, <<"jid">>),
                      Pref = escalus_utils:is_prefix(ContactJid, ExpectedJid),
                      if %% TODO: simplify to `ContactJid == ExpectedJid`
                          ContactJid == ExpectedJid ->
                              true;
                          Pref ->
                              escalus_compat:complain("deprecated use of escalus_pred:roster_contains"),
                              true;
                          true ->
                              false
                      end;
                  (_CData) ->
                      false
              end, Items).

count_roster_items(Num, Stanza) ->
    Num == length(get_roster_items(Stanza)).

is_error(Type, Condition, Stanza) ->
    Error = exml_query:subelement(Stanza, <<"error">>),
    has_type(<<"error">>, Stanza)
    andalso
    exml_query:attr(Error, <<"type">>) == bin(Type)
    andalso
    exml_query:path(Error, [{element, bin(Condition)},
                            {attr, <<"xmlns">>}]) == ?NS_STANZA_ERRORS.

is_stream_error(Type, Text, Stanza) ->
    Stanza#xmlel.name =:= <<"stream:error">>
    andalso
    exml_query:subelement(Stanza, Type) =/= undefined
    andalso
    case Text of
        <<>> -> true;
        _ -> [#xmlcdata{content = Text}]
             =:= (exml_query:subelement(Stanza, <<"text">>))#xmlel.children
    end.

is_privacy_set(Stanza) ->
    is_iq(<<"set">>, ?NS_PRIVACY, Stanza).

is_privacy_result(Stanza) ->
    is_iq(<<"result">>, ?NS_PRIVACY, Stanza).

is_privacy_result_with(Child, Stanza) ->
    is_privacy_result(Stanza)
    andalso
    has_path(Stanza, [{element, <<"query">>},
                      {element, Child}]).

is_privacy_result_with(Child, ChildName, Stanza) ->
    is_privacy_result(Stanza)
    andalso
    ChildName == exml_query:path(Stanza, [{element, <<"query">>},
                                          {element, Child},
                                          {attr, <<"name">>}]).

is_privacy_result_with_active(Stanza) ->
    is_privacy_result_with(<<"active">>, Stanza).

is_privacy_result_with_default(Stanza) ->
    is_privacy_result_with(<<"default">>, Stanza).

is_privacy_result_with_active(ActiveListName, Stanza) ->
    is_privacy_result_with(<<"active">>, ActiveListName, Stanza).

is_privacy_result_with_default(DefaultListName, Stanza) ->
    is_privacy_result_with(<<"default">>, DefaultListName, Stanza).

is_privacy_list_nonexistent_error(Stanza) ->
    is_iq(<<"error">>, ?NS_PRIVACY, Stanza)
    andalso
    has_path(Stanza, [{element, <<"query">>},
                      {element, <<"list">>},
                      {attr, <<"name">>}])
    andalso
    has_path(Stanza, [{element, <<"error">>},
                      {element, <<"item-not-found">>}]).

is_adhoc_response(Node, Status, Stanza) ->
    is_iq(Stanza)
        andalso
        ?NS_ADHOC == exml_query:path(Stanza, [{element, <<"command">>},
                                              {attr, <<"xmlns">>}])
        andalso
        Node == exml_query:path(Stanza, [{element, <<"command">>},
                                         {attr, <<"node">>}])
        andalso
        Status == exml_query:path(Stanza, [{element, <<"command">>},
                                           {attr, <<"status">>}]).

has_service(Service, #xmlel{children = [ #xmlel{children = Services} ]}) ->
    Pred = fun(Item) ->
               exml_query:attr(Item, <<"jid">>) =:= Service
           end,
    lists:any(Pred, Services).

has_feature(Feature, Stanza) ->
    Features = exml_query:paths(Stanza, [{element, <<"query">>},
                                         {element, <<"feature">>}]),
    lists:any(fun(Item) ->
                      exml_query:attr(Item, <<"var">>) == Feature
              end,
              Features).

has_item(JID, Stanza) ->
    Items = exml_query:paths(Stanza, [{element, <<"query">>},
                                      {element, <<"item">>}]),
    lists:any(fun(Item) ->
                      exml_query:attr(Item, <<"jid">>) == JID
              end,
              Items).

has_no_such_item(JID, Stanza) ->
    not has_item(JID, Stanza).

has_identity(Category, Type, Stanza) ->
    Idents = exml_query:paths(Stanza, [{element, <<"query">>},
                                       {element, <<"identity">>}]),
    lists:any(fun(Ident) ->
                      (exml_query:attr(Ident, <<"category">>) == Category)
                          and (exml_query:attr(Ident, <<"type">>) == Type)
              end,
              Idents).

stanza_timeout(Arg) ->
    case element(1, Arg) of
        'EXIT' ->
            case element(1, element(2, Arg)) of
                timeout_when_waiting_for_stanza ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.

is_stream_end(#xmlstreamend{}) ->
    true;
is_stream_end(_) ->
    false.

is_bosh_report(Rid, #xmlel{name = <<"body">>} = Body) ->
    Rid == list_to_integer(binary_to_list(exml_query:attr(Body, <<"report">>)))
    andalso
    exml_query:attr(Body, <<"time">>) /= undefined;
is_bosh_report(_, _) ->
    false.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_roster_items(Stanza) ->
    escalus:assert(is_iq_with_ns, [?NS_ROSTER], Stanza),
    Query = exml_query:subelement(Stanza, <<"query">>),
    Query#xmlel.children.

has_path(Stanza, Path) ->
    exml_query:path(Stanza, Path) /= undefined.
