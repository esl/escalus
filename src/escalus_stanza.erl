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
         presence_show/1,
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
         auth_response_stanza/1,
         query_el/2,
         x_data_form/2
     ]).

-export([disco_info/1,
         disco_items/1]).

-export([vcard_update/1,
         vcard_update/2,
         vcard_request/0,
         vcard_request/1,
         search_fields/1,
         search_fields_iq/1,
         search_iq/2]).

-export([stream_start/2,
         stream_end/0,
         starttls/0,
         compress/1]).

-export([iq/2, iq/3]).
-export([bind/1, session/0]).

%% new ones
-export([setattr/3, to/2, tags/1]).
-export([get_registration_fields/0, register_account/1]).
-export([remove_account/0]).

-import(escalus_compat, [bin/1]).

-include("escalus.hrl").
-include("escalus_xmlns.hrl").
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
    #xmlel{name = <<"starttls">>,
                attrs=[
                    {<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-tls">>}
                ]}.

compress(Method) ->
    #xmlel{name = <<"compress">>,
                attrs = [
                    {<<"xmlns">>, <<"http://jabber.org/protocol/compress">>}
                ],
                children = [
                    #xmlel{name = <<"method">>, children = [exml:escape_cdata(Method)]}
                ]}.

-spec iq(binary(), [xmlterm()]) -> #xmlel{}.
iq(Type, Body) ->
    #xmlel{name = <<"iq">>,
                attrs=[{<<"type">>, Type},
                       {<<"id">>, id()}],
                children = Body}.

iq(To, Type, Body) ->
    IQ = iq(Type, Body),
    IQ#xmlel{ attrs = [{<<"to">>, To} | IQ#xmlel.attrs] }.

%% slightly naughty, this isn't a stanza but it will go in an <iq/>
query_el(NS, Children) ->
        #xmlel{ name = <<"query">>,
                     attrs = [{<<"xmlns">>, NS}],
                     children = Children
                   }.

%% http://xmpp.org/extensions/xep-0004.html
%% slightly naughty - this isn't a stanza but can be a child of various stanza types
x_data_form(Type, Children) ->
    #xmlel{ name = <<"x">>,
                 attrs = [{<<"xmlns">>,?NS_DATA_FORMS},
                          {<<"type">>, Type}],
                 children = Children
               }.

-spec bind(binary()) -> #xmlel{}.
bind(Resource) ->
    NS = <<"urn:ietf:params:xml:ns:xmpp-bind">>,
    iq(<<"set">>, [
        #xmlel{name = <<"bind">>, attrs = [{<<"xmlns">>, NS}], children=[
            #xmlel{name = <<"resource">>, children=[exml:escape_cdata(Resource)]}
        ]}]).

-spec session() -> #xmlel{}.
session() ->
    NS = <<"urn:ietf:params:xml:ns:xmpp-session">>,
    iq(<<"set">>, #xmlel{name = <<"session">>, attrs = [{<<"xmlns">>, NS}]}).

to(Stanza, Recipient) when is_binary(Recipient) ->
    setattr(Stanza, <<"to">>, Recipient);
to(Stanza, Recipient) ->
    setattr(Stanza, <<"to">>, escalus_utils:get_jid(Recipient)).

setattr(Stanza, Key, Val) ->
    NewAttrs = lists:keystore(Key, 1, Stanza#xmlel.attrs, {Key, Val}),
    Stanza#xmlel{attrs = NewAttrs}.

tags(KVs) ->
    [#xmlel{name = K, children = [exml:escape_cdata(V)]} || {K, V} <- KVs].

presence(Type) ->
    presence(Type, []).

presence(<<"available">>, Body) ->
    #xmlel{name = <<"presence">>, children = Body};
presence(Type, Body) ->
    #xmlel{name = <<"presence">>,
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

presence_show(Show) ->
    presence(<<"available">>,
             #xmlel{name = <<"show">>, children=[#xmlcdata{content = Show}]}).

error_element(Type, Condition) ->
    #xmlel{
        name = <<"error">>,
        attrs = [{<<"type">>, Type}],
        children = #xmlel{
            name = Condition,
            attrs = [{<<"xmlns">>, ?NS_STANZA_ERRORS}]
        }}.

message(From, Recipient, Type, Msg) ->
    FromAttr = case From of
                   undefined -> [];
                   _ -> [{<<"from">>, From}]
               end,
    #xmlel{
       name = <<"message">>,
       attrs = FromAttr ++
           [{<<"type">>, Type},
            {<<"to">>, escalus_utils:get_jid(Recipient)}
           ],
       children = [#xmlel{
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
        #xmlel{name = <<"query">>, attrs = [
            {<<"xmlns">>, <<"jabber:iq:register">>}
        ]}
    ]).

register_account(Body) ->
    iq(<<"set">>, [
        #xmlel{name = <<"query">>, attrs = [
            {<<"xmlns">>, <<"jabber:iq:register">>}
        ], children = Body}
    ]).

remove_account() ->
    iq(<<"set">>, [
        #xmlel{name = <<"query">>, attrs = [
            {<<"xmlns">>, <<"jabber:iq:register">>}
        ], children = [#xmlel{name = <<"remove">>}]}
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
    #xmlel{name = <<"iq">>,
                attrs = Attrs}.

iq_get(NS, Payload) ->
    iq_with_type(<<"get">>, NS, Payload).

iq_set(NS, Payload) ->
    iq_with_type(<<"set">>, NS, Payload).

iq_with_type(Type, NS, Payload) ->
    iq(Type, [#xmlel{
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
            #xmlel{
                name = <<"item">>,
                attrs = [{<<"jid">>, escalus_utils:get_short_jid(User)}, %% XXX
                         {<<"name">>, bin(Nick)}],
                children = [
                    #xmlel{name = <<"group">>,
                                children = [exml:escape_cdata(bin(Group))]}
                    || Group <- Groups]}
        end,
        ItemSpecs),
    iq_set(?NS_ROSTER, Items).

roster_add_contact(User, Groups, Nick) ->
    roster_add_contacts([{User, Groups, Nick}]).

%% FIXME: see roster_add_contacts/1 comment
roster_remove_contact(User) ->
    iq_set(?NS_ROSTER, [#xmlel{
        name = <<"item">>,
        attrs = [{<<"jid">>, escalus_utils:get_short_jid(User)}, %% XXX
                 {<<"subscription">>, <<"remove">>}]
    }]).

private_set(Element) ->
    iq_set(?NS_PRIVATE, [Element]).

private_get(NS, Name) ->
    Element = #xmlel{name = bin(Name),
                          attrs = [{<<"xmlns">>, bin(NS)}]},
    iq_get(?NS_PRIVATE, [Element]).

last_activity(User) ->
    to(iq_get(?NS_LAST_ACTIVITY, []), User).

privacy_get_all() ->
    iq_get(?NS_PRIVACY, []).

privacy_get_lists(ListNames) ->
    iq_get(?NS_PRIVACY, [#xmlel{name = <<"list">>,
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
    iq_set(?NS_PRIVACY, [#xmlel{name = What, attrs = Attrs}]).

%% Create empty list element with given name.
privacy_list(Name, Items) ->
    #xmlel{name = <<"list">>,
                attrs = [{<<"name">>, Name}],
                children = Items}.

privacy_list_item(Order, Action, Content) ->
    #xmlel{name = <<"item">>,
                attrs = [{<<"order">>, Order},
                         {<<"action">>, Action}],
                children = [#xmlel{name = C} || C <- Content]}.

privacy_list_item(Order, Action, Type, Value, Content) ->
    #xmlel{name = <<"item">>,
                attrs = [{<<"order">>, Order},
                         {<<"type">>, Type},
                         {<<"value">>, Value},
                         {<<"action">>, Action}],
                children = [#xmlel{name = C} || C <- Content]}.

privacy_list_jid_item(Order, Action, Who, Contents) ->
    privacy_list_item(Order, Action, <<"jid">>,
                      escalus_utils:get_jid(Who), Contents).

disco_info(JID) ->
    Query = query_el(?NS_DISCO_INFO, []),
    iq(JID, <<"get">>, [Query]).

disco_items(JID) ->
    ItemsQuery = query_el(?NS_DISCO_ITEMS, []),
    iq(JID, <<"get">>, [ItemsQuery]).

search_fields([]) ->
    [];
search_fields([null|Rest]) ->
    [#xmlel{name = <<"field">>} | search_fields(Rest)];
search_fields([{Key, Val}|Rest]) ->
    [#xmlel{
            name = <<"field">>,
            attrs = [{<<"var">>, Key}],
            children = [
                #xmlel{
                    name = <<"value">>,
                    children = [
                        {xmlcdata, Val}]}]}
     | search_fields(Rest) ].

search_fields_iq(JID) ->
    iq(JID, <<"get">>, [
        query_el(?NS_SEARCH, [])]).

search_iq(JID, Fields) ->
    Form = x_data_form(<<"submit">>, Fields),
    Query = query_el(?NS_SEARCH, [Form]),
    iq(JID, <<"set">>, [Query]).

vcard_request() ->
    iq(<<"get">>, [vcard([])]).

vcard_request(JID) ->
    iq(JID, <<"get">>, [vcard([])]).

vcard_update(Fields) ->
    iq(<<"set">>, [vcard(Fields)]).

vcard_update(JID, Fields) ->
    iq(JID, <<"set">>, [vcard(Fields)]).

vcard([{_,_}|_] = Tuples) ->
    vcard(tuples_to_fields(Tuples));
vcard(Body) ->
    #xmlel{
        name = <<"vCard">>,
        attrs = [{<<"xmlns">>,<<"vcard-temp">>}],
        children = Body
    }.

cdata_field(Name, Value) ->
    #xmlel{name = Name,
                attrs = [],
                children = [{xmlcdata, Value}]}.

field(Name, Children) ->
    #xmlel{name = Name,
                attrs = [],
                children = Children}.

tuples_to_fields([]) ->
    [];
tuples_to_fields([{Name, Value}|Rest]) when is_binary(Value) ->
    [cdata_field(Name, Value) | tuples_to_fields(Rest)];
tuples_to_fields([{Name, Children}|Rest]) when is_list(Children) ->
    [field(Name, tuples_to_fields(Children))
        | tuples_to_fields(Rest)].

adhoc_request(Node) ->
    adhoc_request(Node, []).

adhoc_request(Node, Payload) ->
    iq(<<"set">>, [#xmlel{name = <<"command">>,
                               attrs = [{<<"xmlns">>, ?NS_ADHOC},
                                        {<<"node">>, Node},
                                        {<<"action">>, <<"execute">>}],
                               children = Payload}]).

service_discovery(Server) ->
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), <<"to">>,
                           Server).

auth_stanza(Mechanism, Body) ->
    #xmlel{name = <<"auth">>,
                attrs = [{<<"xmlns">>, ?NS_SASL},
                         {<<"mechanism">>, Mechanism}],
                children = Body}.

auth_response_stanza(Body) ->
    #xmlel{name = <<"response">>,
                attrs = [{<<"xmlns">>, ?NS_SASL}],
                children = Body}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec id() -> binary().
id() ->
    base16:encode(crypto:rand_bytes(16)).

