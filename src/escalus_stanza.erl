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

%% old ones
-export([chat_to/2,
         chat_to_short_jid/2,
         groupchat_to/2,
         iq_result/1,
         iq_get/2,
         iq_set/2,
         presence/1,
         presence/2,
         presence_direct/2,
         presence_direct/3,
         error_element/2,
         roster_get/0,
         roster_add_contact/3,
         roster_remove_contact/1,
         private_set/1,
         private_get/2,
         last_activity/1,
         privacy_list/2,
         privacy_list_item/3,
         privacy_list_item/5,
         privacy_list_jid_item/4,
         privacy_get_all/0,
         privacy_get_lists/1,
         privacy_set_list/1,
         privacy_activate/1,
         privacy_deactivate/0,
         privacy_set_default/1,
         privacy_no_default/0
     ]).

%% new ones
-export([setattr/3, to/2, tags/1]).
-export([get_registration_fields/0, register_account/1]).
-export([remove_account/0]).

-import(escalus_compat, [bin/1]).

-include("escalus.hrl").
-include("escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

to(Stanza, Recipient) ->
    setattr(Stanza, <<"to">>, escalus_utils:get_jid(Recipient)).

setattr(Stanza, Key, Val) ->
    NewAttrs = lists:keystore(Key, 1, Stanza#xmlelement.attrs, {Key, Val}),
    Stanza#xmlelement{attrs = NewAttrs}.

tags(KVs) ->
    [#xmlelement{name = K, body = [exml:escape_cdata(V)]} || {K, V} <- KVs].

presence(Type) ->
    presence(Type, []).

presence(<<"available">>, Body) ->
    #xmlelement{name = <<"presence">>, body = Body};
presence(Type, Body) ->
    #xmlelement{name = <<"presence">>,
                attrs = [{<<"type">>, bin(Type)}],
                body = Body}.

presence_direct(Recipient, Type) ->
    presence_direct(Recipient, Type, []).

presence_direct(#client{} = Recipient, Type, Body) ->
    %% FIXME: this clause is only for backwards compatibility,
    %% remove at some point
    BType = bin(Type),
    if
        BType == <<"subscribe">>;
        BType == <<"subscribed">>;
        BType == <<"unsubscribe">>;
        BType == <<"unsubscribed">> ->
            escalus_compat:complain("possibly bad use of "
                                    "presence_direct with full JID"),
            presence_direct(escalus_utils:get_short_jid(Recipient), BType, Body);
        true ->
            to(presence(Type, Body), Recipient)
    end;
presence_direct(Recipient, Type, Body) ->
    to(presence(Type, Body), Recipient).

error_element(Type, Condition) ->
    #xmlelement{
        name = <<"error">>,
        attrs = [{<<"type">>, Type}],
        body = #xmlelement{
            name = Condition,
            attrs = [{<<"xmlns">>, ?NS_STANZA_ERRORS}]
        }}.

message_to(Recipient, Type, Msg) ->
    #xmlelement{name = <<"message">>, attrs = [
        {<<"type">>, Type},
        {<<"to">>, escalus_utils:get_jid(Recipient)}
    ], body = [#xmlelement{name = <<"body">>, body = [
        exml:escape_cdata(Msg)
    ]}]}.

chat_to(Recipient, Msg) ->
    message_to(Recipient, <<"chat">>, Msg).

chat_to_short_jid(Recipient, Msg) ->
    chat_to(escalus_utils:get_short_jid(Recipient), Msg).

groupchat_to(Recipient, Msg) ->
    message_to(Recipient, <<"groupchat">>, Msg).

get_registration_fields() ->
    lxmppc_stanza:iq(<<"get">>, [
        #xmlelement{name = <<"query">>, attrs = [
            {<<"xmlns">>, <<"jabber:iq:register">>}
        ]}
    ]).

register_account(Body) ->
    lxmppc_stanza:iq(<<"set">>, [
        #xmlelement{name = <<"query">>, attrs = [
            {<<"xmlns">>, <<"jabber:iq:register">>}
        ], body = Body}
    ]).

remove_account() ->
    lxmppc_stanza:iq(<<"set">>, [
        #xmlelement{name = <<"query">>, attrs = [
            {<<"xmlns">>, <<"jabber:iq:register">>}
        ], body = [#xmlelement{name = <<"remove">>}]}
    ]).

iq_result(Request) ->
    ToAttr = case exml_query:attr(Request, <<"from">>) of
                 undefiend ->
                     [];
                 Jid ->
                     [{<<"to">>, Jid}]
             end,
    Id = exml_query:attr(Request, <<"id">>),
    Attrs = ToAttr ++ [{<<"id">>, Id}, {<<"type">>, <<"result">>}],
    #xmlelement{name = <<"iq">>,
                attrs = Attrs}.

iq_get(NS, Payload) ->
    iq_with_type(<<"get">>, NS, Payload).

iq_set(NS, Payload) ->
    iq_with_type(<<"set">>, NS, Payload).

iq_with_type(Type, NS, Payload) ->
    lxmppc_stanza:iq(Type, [#xmlelement{
        name = <<"query">>,
        attrs = [{<<"xmlns">>, NS}],
        body = Payload
    }]).

roster_get() ->
    iq_get(?NS_ROSTER, []).

%% FIXME: there is a legacy issue here. This function should
%% use get_jid function to let the caller make decision
%% wether to use bare or full jid.
roster_add_contact(User, Groups, Nick) ->
    iq_set(?NS_ROSTER, [#xmlelement{
        name = <<"item">>,
        attrs = [{<<"jid">>, escalus_utils:get_short_jid(User)}, %% XXX
                 {<<"name">>, bin(Nick)}],
        body = [
            #xmlelement{name = <<"group">>,
                        body = [exml:escape_cdata(bin(Group))]}
            || Group <- Groups]
    }]).

%% FIXME: see roster_add_contact/3 comment
roster_remove_contact(User) ->
    iq_set(?NS_ROSTER, [#xmlelement{
        name = <<"item">>,
        attrs = [{<<"jid">>, escalus_utils:get_short_jid(User)}, %% XXX
                 {<<"subscription">>, <<"remove">>}]
    }]).

private_set(Element) ->
    iq_set(?NS_PRIVATE, [Element]).

private_get(NS, Name) ->
    Element = #xmlelement{name = bin(Name),
                          attrs = [{<<"xmlns">>, bin(NS)}]},
    iq_get(?NS_PRIVATE, [Element]).

last_activity(User) ->
    to(iq_get(?NS_LAST_ACTIVITY, []), User).

privacy_get_all() ->
    iq_get(?NS_PRIVACY, []).

privacy_get_lists(ListNames) ->
    iq_get(?NS_PRIVACY, [#xmlelement{name = <<"list">>,
                                     attrs = [{<<"name">>, bin(Name)}]}
                         || Name <- ListNames]).

privacy_set_list(PrivacyList) ->
    iq_set(?NS_PRIVACY, [PrivacyList]).

privacy_activate(ListName) ->
    privacy_set(<<"active">>, [{<<"name">>, bin(ListName)}]).

privacy_deactivate()->
    privacy_set(<<"active">>, []).

privacy_set_default(ListName) ->
    privacy_set(<<"default">>, [{<<"name">>, bin(ListName)}]).

privacy_no_default()->
    privacy_set(<<"default">>, []).

privacy_set(What, Attrs) ->
    iq_set(?NS_PRIVACY, [#xmlelement{name = What, attrs = Attrs}]).

%% Create empty list element with given name.
privacy_list(Name, Items) ->
    #xmlelement{name = <<"list">>,
                attrs = [{<<"name">>, Name}],
                body = Items}.

privacy_list_item(Order, Action, Content) ->
    #xmlelement{name = <<"item">>,
                attrs = [{<<"order">>, Order},
                         {<<"action">>, Action}],
                body = [#xmlelement{name = C} || C <- Content]}.

privacy_list_item(Order, Action, Type, Value, Content) ->
    #xmlelement{name = <<"item">>,
                attrs = [{<<"order">>, Order},
                         {<<"type">>, Type},
                         {<<"value">>, Value},
                         {<<"action">>, Action}],
                body = [#xmlelement{name = C} || C <- Content]}.

privacy_list_jid_item(Order, Action, Who, Contents) ->
    privacy_list_item(Order, Action, <<"jid">>,
                      escalus_utils:get_jid(Who), Contents).
