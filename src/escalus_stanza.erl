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
         chat/3,
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
         roster_add_contacts/1,
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
         privacy_no_default/0,
         adhoc_request/1,
         adhoc_request/2,
         service_discovery/1,
         auth_stanza/2,
         auth_response_stanza/1
     ]).

-export([stream_start/2,
         stream_end/0,
         starttls/0,
         compress/1]).

-export([iq/2]).
-export([bind/1, session/0]).

%% new ones
-export([setattr/3, to/2, tags/1]).
-export([get_registration_fields/0, register_account/1]).
-export([remove_account/0]).

-import(escalus_compat, [bin/1]).

-include("include/escalus.hrl").
-include("include/escalus_xmlns.hrl").
-include_lib("exml/include/exml_stream.hrl").

%%--------------------------------------------------------------------
%% Stream - related functions
%%--------------------------------------------------------------------

stream_start(Server, XMLNS) ->
    #xmlstreamstart{name = <<"stream:stream">>, attrs=[
            {<<"to">>, Server},
            {<<"version">>, <<"1.0">>},
            {<<"xml:lang">>, <<"en">>},
            {<<"xmlns">>, XMLNS},
            {<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>}]}.

stream_end() ->
    #xmlstreamend{name = <<"stream:stream">>}.

starttls() ->
    #xmlelement{name = <<"starttls">>,
                attrs=[
                    {<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-tls">>}
                ]}.

compress(Method) ->
    #xmlelement{name = <<"compress">>,
                attrs = [
                    {<<"xmlns">>, <<"http://jabber.org/protocol/compress">>}
                ],
                children = [
                    #xmlelement{name = <<"method">>, children = [exml:escape_cdata(Method)]}
                ]}.

-spec iq(binary(), [xmlterm()]) -> #xmlelement{}.
iq(Type, Body) ->
    #xmlelement{name = <<"iq">>,
                attrs=[{<<"type">>, Type},
                       {<<"id">>, id()}],
                children = Body}.

-spec bind(binary()) -> #xmlelement{}.
bind(Resource) ->
    NS = <<"urn:ietf:params:xml:ns:xmpp-bind">>,
    iq(<<"set">>, [
        #xmlelement{name = <<"bind">>, attrs = [{<<"xmlns">>, NS}], children=[
            #xmlelement{name = <<"resource">>, children=[exml:escape_cdata(Resource)]}
        ]}]).

-spec session() -> #xmlelement{}.
session() ->
    NS = <<"urn:ietf:params:xml:ns:xmpp-session">>,
    iq(<<"set">>, #xmlelement{name = <<"session">>, attrs = [{<<"xmlns">>, NS}]}).

to(Stanza, Recipient) ->
    setattr(Stanza, <<"to">>, escalus_utils:get_jid(Recipient)).

setattr(Stanza, Key, Val) ->
    NewAttrs = lists:keystore(Key, 1, Stanza#xmlelement.attrs, {Key, Val}),
    Stanza#xmlelement{attrs = NewAttrs}.

tags(KVs) ->
    [#xmlelement{name = K, children = [exml:escape_cdata(V)]} || {K, V} <- KVs].

presence(Type) ->
    presence(Type, []).

presence(<<"available">>, Body) ->
    #xmlelement{name = <<"presence">>, children = Body};
presence(Type, Body) ->
    #xmlelement{name = <<"presence">>,
                attrs = [{<<"type">>, bin(Type)}],
                children = Body}.

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
        children = #xmlelement{
            name = Condition,
            attrs = [{<<"xmlns">>, ?NS_STANZA_ERRORS}]
        }}.

message(From, Recipient, Type, Msg) ->
    FromAttr = case From of
                   undefined -> [];
                   _ -> [{<<"from">>, From}]
               end,
    #xmlelement{
       name = <<"message">>,
       attrs = FromAttr ++
           [{<<"type">>, Type},
            {<<"to">>, escalus_utils:get_jid(Recipient)}
           ],
       children = [#xmlelement{
                      name = <<"body">>,
                      children = [exml:escape_cdata(Msg)]
                     }
                  ]
      }.

chat_to(Recipient, Msg) ->
    message(undefined, Recipient, <<"chat">>, Msg).

chat(Sender, Recipient, Msg) ->
    message(Sender, Recipient, <<"chat">>, Msg).

chat_to_short_jid(Recipient, Msg) ->
    chat_to(escalus_utils:get_short_jid(Recipient), Msg).

groupchat_to(Recipient, Msg) ->
    message(undefined, Recipient, <<"groupchat">>, Msg).

get_registration_fields() ->
    iq(<<"get">>, [
        #xmlelement{name = <<"query">>, attrs = [
            {<<"xmlns">>, <<"jabber:iq:register">>}
        ]}
    ]).

register_account(Body) ->
    iq(<<"set">>, [
        #xmlelement{name = <<"query">>, attrs = [
            {<<"xmlns">>, <<"jabber:iq:register">>}
        ], children = Body}
    ]).

remove_account() ->
    iq(<<"set">>, [
        #xmlelement{name = <<"query">>, attrs = [
            {<<"xmlns">>, <<"jabber:iq:register">>}
        ], children = [#xmlelement{name = <<"remove">>}]}
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
    iq(Type, [#xmlelement{
        name = <<"query">>,
        attrs = [{<<"xmlns">>, NS}],
        children = Payload
    }]).

roster_get() ->
    iq_get(?NS_ROSTER, []).

%% FIXME: there is a legacy issue here. This function should
%% use get_jid function to let the caller make decision
%% whether to use bare or full jid.
roster_add_contacts(ItemSpecs) ->
    Items = lists:map(
        fun({User, Groups, Nick}) ->
            #xmlelement{
                name = <<"item">>,
                attrs = [{<<"jid">>, escalus_utils:get_short_jid(User)}, %% XXX
                         {<<"name">>, bin(Nick)}],
                children = [
                    #xmlelement{name = <<"group">>,
                                children = [exml:escape_cdata(bin(Group))]}
                    || Group <- Groups]}
        end,
        ItemSpecs),
    iq_set(?NS_ROSTER, Items).

roster_add_contact(User, Groups, Nick) ->
    roster_add_contacts([{User, Groups, Nick}]).

%% FIXME: see roster_add_contacts/1 comment
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
                children = Items}.

privacy_list_item(Order, Action, Content) ->
    #xmlelement{name = <<"item">>,
                attrs = [{<<"order">>, Order},
                         {<<"action">>, Action}],
                children = [#xmlelement{name = C} || C <- Content]}.

privacy_list_item(Order, Action, Type, Value, Content) ->
    #xmlelement{name = <<"item">>,
                attrs = [{<<"order">>, Order},
                         {<<"type">>, Type},
                         {<<"value">>, Value},
                         {<<"action">>, Action}],
                children = [#xmlelement{name = C} || C <- Content]}.

privacy_list_jid_item(Order, Action, Who, Contents) ->
    privacy_list_item(Order, Action, <<"jid">>,
                      escalus_utils:get_jid(Who), Contents).

adhoc_request(Node) ->
    adhoc_request(Node, []).

adhoc_request(Node, Payload) ->
    iq(<<"set">>, [#xmlelement{name = <<"command">>,
                               attrs = [{<<"xmlns">>, ?NS_ADHOC},
                                        {<<"node">>, Node},
                                        {<<"action">>, <<"execute">>}],
                               children = Payload}]).

service_discovery(Server) ->
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), <<"to">>,
                           Server).

auth_stanza(Mechanism, Body) ->
    #xmlelement{name = <<"auth">>,
                attrs = [{<<"xmlns">>, ?NS_SASL},
                         {<<"mechanism">>, Mechanism}],
                children = Body}.

auth_response_stanza(Body) ->
    #xmlelement{name = <<"response">>,
                attrs = [{<<"xmlns">>, ?NS_SASL}],
                children = Body}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec id() -> binary().
id() ->
    base16:encode(crypto:rand_bytes(16)).

