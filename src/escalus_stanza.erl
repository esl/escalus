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
-export([id/0,
         message/2, message/4,
         chat_to/2,
         chat/3,
         chat_to_short_jid/2,
         chat_without_carbon_to/2,
         chat_to_with_id_and_timestamp/2,
         chat_to_with_id/2,
         groupchat_to/2,
         iq_result/1,
         iq_result/2,
         iq_get/2,
         iq_set/2,
         iq_set_nonquery/2,
         presence/1,
         presence/2,
         presence_direct/2,
         presence_direct/3,
         presence_show/1,
         error_element/2,
         receipt_req/1,
         receipt_conf/1,
         roster_get/0,
         roster_get/1,
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
         ping_request/1,
         service_discovery/1,
         auth/1,
         auth/2,
         auth_response/0,
         auth_response/1,
         query_el/2,
         query_el/3,
         x_data_form/2,
         field_el/3]).

-export([disco_info/1,
         disco_info/2,
         disco_items/1,
         disco_items/2
        ]).

-export([vcard_update/1,
         vcard_update/2,
         vcard_request/0,
         vcard_request/1,
         search_fields/1,
         search_fields_iq/1,
         search_iq/2]).

%% XEP-0280: Message Carbons
-export([carbons_disable/0,carbons_enable/0]).

%% XEP-0313: Message Archive Management
-export([mam_archive_query/1,
         mam_lookup_messages_iq/4,
         mam_lookup_messages_iq/5,
         mam_lookup_messages_iq/6
        ]).

%% XEP-0333: Chat Markers
-export([markable/1,
         chat_marker/3]).

%% XEP-0198: Stream Management
-export([enable_sm/0, enable_sm/1,
         sm_request/0,
         sm_ack/1,
         resume/2]).

-export([stream_start/2,
         stream_end/0,
         ws_open/1,
         ws_close/0,
         starttls/0,
         compress/1]).

-export([iq/2, iq/3]).

-export([bind/1,
         session/0]).

-export([setattr/3,
         to/2,
         from/2,
         tags/1,
         set_id/2,
         uuid_v4/0]).

-export([get_registration_fields/0,
         register_account/1]).

-export([remove_account/0]).

%% Stanzas from inline XML
-export([from_template/2,
         from_xml/1]).

-import(escalus_compat, [bin/1]).

-include("escalus.hrl").
-include("escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").

-define(b2l(B), erlang:binary_to_list(B)).
-define(i2l(I), erlang:integer_to_list(I)).
-define(io2b(IOList), erlang:iolist_to_binary(IOList)).

%%--------------------------------------------------------------------
%% Stream - related functions
%%--------------------------------------------------------------------

stream_start(Server, XMLNS) ->
    #xmlstreamstart{name = <<"stream:stream">>,
                    attrs = [{<<"to">>, Server},
                             {<<"version">>, <<"1.0">>},
                             {<<"xml:lang">>, <<"en">>},
                             {<<"xmlns">>, XMLNS},
                             {<<"xmlns:stream">>,
                              <<"http://etherx.jabber.org/streams">>}]}.

stream_end() ->
    #xmlstreamend{name = <<"stream:stream">>}.

ws_open(Server) ->
    #xmlel{name= <<"open">>, attrs = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-framing">>},
                                      {<<"to">>, Server},
                                      {<<"version">>,<<"1.0">>}]}.

ws_close()->
    #xmlel{name= <<"close">>, attrs = [ {<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-framing">>} ]}.

starttls() ->
    #xmlel{name = <<"starttls">>,
           attrs = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-tls">>}]}.

compress(Method) ->
    #xmlel{name = <<"compress">>,
           attrs = [{<<"xmlns">>, <<"http://jabber.org/protocol/compress">>}],
           children = [#xmlel{name = <<"method">>,
                              children = [#xmlcdata{content = Method}]}]}.

-spec iq(binary(), [exml:element()]) -> exml:element().
iq(Type, Body) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"type">>, Type},
                    {<<"id">>, id()}],
           children = Body}.

iq(To, Type, Body) ->
    IQ = iq(Type, Body),
    IQ#xmlel{attrs = [{<<"to">>, To} | IQ#xmlel.attrs]}.

%% slightly naughty, this isn't a stanza but it will go inside an <iq/>
query_el(NS, Children) ->
    query_el(NS, [], Children).

query_el(NS, Attrs, Children) ->
    #xmlel{name = <<"query">>,
           attrs = [{<<"xmlns">>, NS} | Attrs],
           children = Children}.

%% http://xmpp.org/extensions/xep-0004.html
%% slightly naughty - this isn't a stanza but can be a child of various stanza types
x_data_form(Type, Children) ->
    #xmlel{name = <<"x">>,
           attrs = [{<<"xmlns">>,?NS_DATA_FORMS},
                    {<<"type">>, Type}],
           children = Children}.

-spec bind(binary()) -> exml:element().
bind(Resource) ->
    Children =
        case Resource of
            <<>> -> [];
            _ -> [#xmlel{name = <<"resource">>, children = [#xmlcdata{content = Resource}]}]
        end,
    iq(<<"set">>,
       [#xmlel{name = <<"bind">>,
               attrs = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-bind">>}],
               children = Children}]).

-spec session() -> exml:element().
session() ->
    NS = <<"urn:ietf:params:xml:ns:xmpp-session">>,
    iq(<<"set">>, [#xmlel{name = <<"session">>,
                          attrs = [{<<"xmlns">>, NS}]}]).

to(Stanza, Recipient) when is_binary(Recipient) ->
    setattr(Stanza, <<"to">>, Recipient);
to(Stanza, Recipient) ->
    setattr(Stanza, <<"to">>, escalus_utils:get_jid(Recipient)).

from(Stanza, Recipient) when is_binary(Recipient) ->
    setattr(Stanza, <<"from">>, Recipient);
from(Stanza, Recipient) ->
    setattr(Stanza, <<"from">>, escalus_utils:get_jid(Recipient)).

set_id(Stanza, ID) ->
    setattr(Stanza, <<"id">>, ID).

setattr(Stanza, Key, Val) ->
    NewAttrs = lists:keystore(Key, 1, Stanza#xmlel.attrs, {Key, Val}),
    Stanza#xmlel{attrs = NewAttrs}.

tags(KVs) ->
    [#xmlel{name = K,
            children = [#xmlcdata{content = V}]} || {K, V} <- KVs].

presence(Type) ->
    presence(Type, []).

-spec presence(binary(), [exml:element()|exml:cdata()]) -> exml:element().
presence(<<"available">>, Children) ->
    #xmlel{name = <<"presence">>, children = Children};
presence(Type, Children) ->
    #xmlel{name = <<"presence">>,
           attrs = [{<<"type">>, bin(Type)}],
           children = Children}.

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
             [#xmlel{name = <<"show">>,
                     children = [#xmlcdata{content = Show}]}]).

error_element(Type, Condition) ->
    #xmlel{name = <<"error">>,
           attrs = [{<<"type">>, Type}],
           children = [#xmlel{name = Condition,
                              attrs = [{<<"xmlns">>, ?NS_STANZA_ERRORS}]}]}.

-spec message(From, Recipient, Type, Msg) -> exml:element() when
      From :: undefined | binary(),
      Recipient :: escalus_utils:jid_spec(),
      Type :: binary(),
      Msg :: binary().
message(From, Recipient, Type, Msg) when is_atom(Recipient) ->
    %% TODO: Drop support for passing just an atom as Recipient,
    %%       therefore removing reliance on users being defined in the config file.
    %%       See escalus_utils:get_jid/1.
    Complaint = io_lib:format("~s:message/4: atom() passed as Recipient - pass "
                              "binary() JID or escalus:client() instead",
                              [?MODULE]),
    escalus_compat:complain(Complaint),
    message1(From, Recipient, Type, Msg);
message(From, Recipient, Type, Msg) ->
    message1(From, Recipient, Type, Msg).

message1(From, Recipient, Type, Msg) ->
    AttrsIn = #{to => Recipient,
                type => Type},
    Attrs = case From of
                 undefined -> AttrsIn;
                 _ -> AttrsIn#{from => From}
             end,
    message(Msg, Attrs).

-spec message(binary(), Attrs) -> exml:element() when
      Attrs :: #{to => escalus_utils:jid_spec(),
                 atom() | binary() => binary()}.
message(Text, Attrs) ->
    maps:fold(fun (to, V, Stanza) -> setattr(Stanza, <<"to">>, escalus_utils:get_jid(V));
                  (K, V, Stanza) when is_atom(K) -> setattr(Stanza, atom_to_binary(K, utf8), V);
                  (K, V, Stanza) when is_binary(K) -> setattr(Stanza, K, V) end,
              #xmlel{name = <<"message">>,
                     children = [#xmlel{name = <<"body">>,
                                        children = [#xmlcdata{content = Text}]}]},
              Attrs).

chat_to(Recipient, Msg) ->
    message(Msg, #{type => <<"chat">>,
                   to => Recipient}).

chat(Sender, Recipient, Msg) ->
    message(Msg, #{type => <<"chat">>,
                   from => Sender,
                   to => Recipient}).

chat_to_short_jid(Recipient, Msg) ->
    message(Msg, #{type => <<"chat">>,
                   to => escalus_utils:get_short_jid(Recipient)}).

-spec chat_to_with_id(escalus_utils:jid_spec(), binary()) -> exml:element().
chat_to_with_id(Recipient, Msg) ->
    message(Msg, #{type => <<"chat">>,
                   to => Recipient,
                   id => uuid_v4()}).

-spec chat_to_with_id_and_timestamp(escalus_utils:jid_spec(), binary()) -> exml:element().
chat_to_with_id_and_timestamp(Recipient, Msg) ->
    message(Msg, #{type => <<"chat">>,
                   to => Recipient,
                   id => uuid_v4(),
                   timestamp => integer_to_binary(os:system_time(microsecond))}).

chat_without_carbon_to(Recipient, Msg) ->
    Stanza = #xmlel{children = Children} = chat_to(Recipient, Msg),
    Stanza#xmlel{children = Children ++
                  [#xmlel{name = <<"private">>,
                          attrs = [{<<"xmlns">>, ?NS_CARBONS_2}]}]}.

receipt_req(#xmlel{ name = <<"message">>,
                    attrs = Attrs,
                    children = Children } = Msg) ->
    ReqStanza = receipt_req_elem(),
    Msg2 = case lists:keysearch(<<"id">>, 1, Attrs) of
        {value, _} ->
            Msg;
        _ ->
            Msg#xmlel{ attrs = [{<<"id">>, id()} | Attrs] }
    end,
    Msg2#xmlel{ children = [ReqStanza | Children] }.

receipt_conf(#xmlel{ attrs = Attrs, children = Children }) ->
    {value, {_, ID}} = lists:keysearch(<<"id">>, 1, Attrs),
    {value, {_, From}} = lists:keysearch(<<"from">>, 1, Attrs),
    Type = case lists:keyfind(<<"type">>, 1, Attrs) of
        false -> <<"chat">>;
        {_, Type0} -> Type0
    end,
    To = case lists:keyfind(<<"received">>, #xmlel.name, Children) of
        #xmlel{ name = <<"received">> } ->
            [Bare|_] = binary:split(From, <<"/">>),
            [_, Server] = binary:split(Bare, <<"@">>),
            Server;
        false ->
            case Type of
                <<"groupchat">> ->
                    [Bare|_] = binary:split(From, <<"/">>),
                    Bare;
                _ ->
                    From
            end
    end,
    #xmlel{ name = <<"message">>,
            attrs = [{<<"to">>, To}, {<<"id">>, id()}, {<<"type">>, Type}],
            children = [receipt_conf_elem(ID)]
          }.

receipt_req_elem() ->
    #xmlel{
        name = <<"request">>,
        attrs = [{<<"xmlns">>, ?NS_RECEIPTS}],
        children = []
        }.

receipt_conf_elem(ID) ->
    #xmlel{
        name = <<"received">>,
        attrs = [{<<"xmlns">>, ?NS_RECEIPTS}, {<<"id">>, ID}],
        children = []
        }.

groupchat_to(Recipient, Msg) ->
    message(undefined, Recipient, <<"groupchat">>, Msg).

get_registration_fields() ->
    iq(<<"get">>, [#xmlel{name = <<"query">>,
                          attrs = [{<<"xmlns">>, <<"jabber:iq:register">>}]}]).

register_account(Body) ->
    iq(<<"set">>, [#xmlel{name = <<"query">>,
                          attrs = [{<<"xmlns">>, <<"jabber:iq:register">>}],
                          children = Body}]).

remove_account() ->
    iq(<<"set">>, [#xmlel{name = <<"query">>,
                          attrs = [{<<"xmlns">>, <<"jabber:iq:register">>}],
                          children = [#xmlel{name = <<"remove">>}]}]).

iq_result(Request) ->
    iq_result(Request, []).

iq_result(Request, Payload) ->
    ToAttr = case exml_query:attr(Request, <<"from">>) of
                 undefined ->
                     [];
                 Jid ->
                     [{<<"to">>, Jid}]
             end,
    Id = exml_query:attr(Request, <<"id">>),
    Attrs = ToAttr ++ [{<<"id">>, Id}, {<<"type">>, <<"result">>}],
    #xmlel{name = <<"iq">>,
           attrs = Attrs,
           children = Payload}.

iq_get(NS, Payload) ->
    iq_with_type(<<"get">>, NS, Payload).

iq_set(NS, Payload) ->
    iq_with_type(<<"set">>, NS, Payload).

iq_set_nonquery(NS, Payload) ->
    %% Don't wrap <iq/> payload with <query/>
    iq_with_type(<<"set">>, NS, Payload, nonquery).

iq_with_type(Type, NS, Payload) ->
    iq(Type, [#xmlel{name = <<"query">>,
                     attrs = [{<<"xmlns">>, NS}],
                     children = Payload}]).

iq_with_type(Type, NS, Payload, nonquery) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"xmlns">>, NS},
                    {<<"type">>, Type},
                    {<<"id">>, id()}],
           children = Payload}.

roster_get() ->
    iq_get(?NS_ROSTER, []).

roster_get(Ver) ->
    #xmlel{children = [Query]} = Stanza = iq_get(?NS_ROSTER, []),
    NewQuery = Query#xmlel{attrs = [{<<"ver">>, Ver} | Query#xmlel.attrs]},
    Stanza#xmlel{children = [NewQuery]}.

roster_add_contacts(ItemSpecs) ->
    iq_set(?NS_ROSTER, lists:map(fun contact_item/1, ItemSpecs)).

%% FIXME: there is a legacy issue here. This function should
%% use get_jid function to let the caller make decision
%% whether to use bare or full jid.
contact_item({User, Groups, Nick}) ->
    #xmlel{name = <<"item">>,
           attrs = [%% XXX
                    {<<"jid">>, escalus_utils:get_short_jid(User)},
                    {<<"name">>, bin(Nick)}],
           children = [#xmlel{name = <<"group">>,
                              children = [#xmlcdata{content = bin(Group)}]}
                       || Group <- Groups]}.

roster_add_contact(User, Groups, Nick) ->
    roster_add_contacts([{User, Groups, Nick}]).

%% FIXME: see contact_item/1 comment
roster_remove_contact(User) ->
    iq_set(?NS_ROSTER,
           [#xmlel{name = <<"item">>,
                   attrs = [%% XXX
                            {<<"jid">>, escalus_utils:get_short_jid(User)},
                            {<<"subscription">>, <<"remove">>}]}]).

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

disco_info(JID, Node) ->
    Query = query_el(?NS_DISCO_INFO, [{<<"node">>, Node}], []),
    iq(JID, <<"get">>, [Query]).

disco_items(JID) ->
    ItemsQuery = query_el(?NS_DISCO_ITEMS, []),
    iq(JID, <<"get">>, [ItemsQuery]).
disco_items(JID, Node) ->
    ItemsQuery = query_el(?NS_DISCO_ITEMS, [{<<"node">>, Node}], []),
    iq(JID, <<"get">>, [ItemsQuery]).

search_fields([]) ->
    [];
search_fields([null|Rest]) ->
    [#xmlel{name = <<"field">>} | search_fields(Rest)];
search_fields([{Key, Val}|Rest]) ->
    [#xmlel{name = <<"field">>,
            attrs = [{<<"var">>, Key}],
            children = [#xmlel{name = <<"value">>,
                               children = [{xmlcdata, Val}]}]}
     | search_fields(Rest)].

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
    #xmlel{name = <<"vCard">>,
           attrs = [{<<"xmlns">>,<<"vcard-temp">>}],
           children = Body}.

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

ping_request(To) ->
    IQ = iq(<<"get">>, [#xmlel{name = <<"ping">>,
                               attrs = [{<<"xmlns">>, ?NS_PING}]
                              }]),
    to(IQ, To).


-spec service_discovery(binary()) -> #xmlel{}.
service_discovery(Server) ->
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), <<"to">>,
                           Server).

-spec auth(binary()) -> #xmlel{}.
auth(Mechanism) ->
    auth(Mechanism, []).

-spec auth(binary(), [#xmlcdata{}]) -> #xmlel{}.
auth(Mechanism, Children) ->
    #xmlel{name = <<"auth">>,
           attrs = [{<<"xmlns">>, ?NS_SASL},
                    {<<"mechanism">>, Mechanism}],
           children = Children}.

auth_response() ->
    auth_response([]).

auth_response(Children) ->
    #xmlel{name = <<"response">>,
           attrs = [{<<"xmlns">>, ?NS_SASL}],
           children = Children}.

enable_sm() ->
    #xmlel{name = <<"enable">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3}]}.

enable_sm(Opts) ->
    #xmlel{name = <<"enable">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3}]
                    ++ [{<<"resume">>, <<"true">>}
                        || true == proplists:is_defined(resume, Opts)]}.

sm_request() ->
    #xmlel{name = <<"r">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3}]}.

sm_ack(H) ->
    #xmlel{name = <<"a">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3},
                    {<<"h">>, integer_to_binary(H)}]}.

resume(SMID, PrevH) ->
    #xmlel{name = <<"resume">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3},
                    {<<"previd">>, SMID},
                    {<<"h">>, integer_to_binary(PrevH)}]}.


%% XEP-0313 Mam
%%
%% @TODO: move the stanza constructors from
%% tests/mam_SUITE.erl into here.
-spec field_el(binary(), binary(), undefined | binary() | [binary()]) ->
    exml:element().
field_el(_Name, _Type, undefined) ->
    undefined;
field_el(Name, Type, Values) when is_list(Values) ->
    Fields = lists:map(fun (E) ->
                               #xmlel{name = <<"value">>,
                                      children = [#xmlcdata{content = E}]}
                       end, Values),
    #xmlel{name = <<"field">>,
           attrs = [{<<"type">>, Type},
                    {<<"var">>, Name}],
           children = Fields };
field_el(Name, Type, Value) ->
    field_el(Name, Type, [Value]).

-spec mam_archive_query(binary()) -> exml:element().
mam_archive_query(QueryId) ->
    mam_archive_query(QueryId, []).

mam_archive_query(QueryId, Children) ->
    DefChilds = defined(Children),

    %%  > 1 -> has not only FORM_TYPE
    ChildEl = case length(DefChilds) > 1 of
                  true ->
                      [#xmlel{name = <<"x">>,
                              attrs = [{<<"xmlns">>, <<"jabberd:x:data">>}],
                              children = DefChilds}];
                  false ->
                      %% no need to create form element
                      []
              end,

    escalus_stanza:iq(
      <<"set">>,
      [#xmlel{
          name = <<"query">>,
          attrs = [mam_ns_attr(), {<<"queryid">>, QueryId}],
          children = ChildEl}]).


-spec mam_lookup_messages_iq(binary(), binary(), binary(), binary()) ->
    exml:element().
mam_lookup_messages_iq(QueryId, Start, End, WithJID) ->
    mam_lookup_messages_iq(QueryId, Start, End, WithJID, false).

-spec mam_lookup_messages_iq(binary(), binary(), binary(), binary(), term()) ->
    exml:element().
mam_lookup_messages_iq(QueryId, Start, End, WithJID, Simple)
  when Simple =:= true; Simple =:= false ->
    Fields = [field_el(<<"FORM_TYPE">>, <<"hidden">>, ?NS_MAM),
              field_el(<<"start">>, <<"text-single">>, Start),
              field_el(<<"end">>, <<"text-single">>, End),
              field_el(<<"with">>, <<"jid-single">>, WithJID)
             ],
    WithSimpleField =
        case Simple of
            true -> [field_el(<<"simple">>, <<"boolean">>, <<"true">>) | Fields];
            false -> Fields
        end,
    mam_archive_query(QueryId, WithSimpleField);
%% Include an rsm id for a particular message.
mam_lookup_messages_iq(QueryId, Start, End, WithJID, DirectionWMessageId) ->
    mam_lookup_messages_iq(QueryId, Start, End, WithJID, DirectionWMessageId, false).

-spec mam_lookup_messages_iq(binary(), binary(), binary(), binary(), term(), boolean()) ->
    exml:element().
mam_lookup_messages_iq(QueryId, Start, End, WithJID, DirectionWMessageId, Simple) ->
    IQ = #xmlel{children=[Q]} = mam_lookup_messages_iq(QueryId, Start, End,
                                                       WithJID, Simple),
    RSM  = defined([fmapM(fun rsm_after_or_before/1, DirectionWMessageId)]),
    Other = Q#xmlel.children,
    Q2 = Q#xmlel{children = Other ++ RSM},
    IQ#xmlel{children=[Q2]}.

fmapM(_F, undefined) -> undefined;
fmapM(F, MaybeVal) -> F(MaybeVal).

defined(L) when is_list(L) -> [ El || El <- L, El /= undefined ].

rsm_after_or_before({Direction, AbstractID, MaxCount}) ->
    #xmlel{name = <<"set">>,
           attrs = [{<<"xmlns">>, ?NS_RSM}],
           children = defined([max(MaxCount), direction_el(Direction, AbstractID) ])}.

direction_el('after', AbstractID) when is_binary(AbstractID) ->
    #xmlel{name = <<"after">>, children = [#xmlcdata{content = AbstractID}]};
direction_el('before', AbstractID) when is_binary(AbstractID) ->
    #xmlel{name = <<"before">>, children = [#xmlcdata{content = AbstractID}]};
direction_el('index', N) when is_integer(N) ->
    #xmlel{name = <<"index">>, children = [#xmlcdata{content = integer_to_binary(N)}]};
direction_el(_, undefined) ->
    undefined.

max(N) when is_integer(N) ->
    #xmlel{name = <<"max">>,
           children = [#xmlcdata{content = integer_to_binary(N)}]};
max(_) ->
    undefined.

mam_ns_attr() -> {<<"xmlns">>,?NS_MAM}.


%% XEP-0280 Carbons
%%
carbons_enable() ->
    iq_set_nonquery(?NS_JABBER_CLIENT, [enable_carbons_el()]).

carbons_disable() ->
    iq_set_nonquery(?NS_JABBER_CLIENT, [disable_carbons_el()]).

disable_carbons_el() ->
    #xmlel{name = <<"disable">>,
           attrs = [{<<"xmlns">>, ?NS_CARBONS_2}]}.

enable_carbons_el() ->
    #xmlel{name = <<"enable">>,
           attrs = [{<<"xmlns">>, ?NS_CARBONS_2}]}.

%% XEP-0333 Chat Markers
%%
-spec markable(exml:element()) -> exml:element().
markable(Message = #xmlel{name = <<"message">>, children = Children}) ->
    Message#xmlel{children = [markable_el() | Children]}.

-spec chat_marker(escalus_utils:jid_spec(), binary(), binary()) -> exml:element().
chat_marker(To, MarkerName, MessageId) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"to">>, escalus_utils:get_jid(To)}],
           children = [marker_el(MarkerName, MessageId)]}.

markable_el() ->
    #xmlel{name = <<"markable">>, attrs = [{<<"xmlns">>, ?NS_CHAT_MARKERS}]}.

marker_el(MarkerName, MessageId) when MarkerName =:= <<"received">> orelse
                                      MarkerName =:= <<"displayed">> orelse
                                      MarkerName =:= <<"acknowledged">>,
                                      is_binary(MessageId) ->
    #xmlel{name = MarkerName, attrs = [{<<"xmlns">>, ?NS_CHAT_MARKERS},
                                       {<<"id">>, MessageId}]}.

-spec id() -> binary().
id() ->
    base16:encode(crypto:strong_rand_bytes(16)).

-spec uuid_v4() -> binary().
uuid_v4() ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard).

%%--------------------------------------------------------------------
%% Stanzas from inline XML
%%--------------------------------------------------------------------

%% @doc An xml_snippet() is a textual representation of XML,
%% possibly with formatting parameters (places where to insert substitutions).
%% It may be a string() or a binary().
%% A parameterless snippet might look like:
%%
%%   <example_element/>
%%
%% Snippet with formatting parameters will look like:
%%
%%   <example_element some_attr="{{attr_value}}"/>
%%
%% Parameter names must be valid atoms, so if you want to use punctuation
%% use single quotes:
%%
%%   <example_element some_attr="{{'fancy:param-name'}}"/>
%%
%% If the argument you pass as the parameter value is an xmlterm()
%% then use triple brackets at the parameter expansion site.
%% Otherwise, the argument term will end up HTML-encoded
%% after expansion.
%%
%%   <example_element>
%%      {{{argument_will_be_xmlterm}}}
%%   </example_element>
%%
%% It's also possible to substitute whole attributes, not just their values:
%%
%%   <example_element {{myattr}}/>
%%
%% Refer to escalus_stanza_SUITE for usage examples.
-type xml_snippet() :: string() | binary().

-spec from_xml(Snippet) -> Term when
      Snippet :: xml_snippet(),
      Term :: exml:element().
from_xml(Snippet) ->
    from_template(Snippet, []).

-type context() :: [{atom(), binary() | list() | exml:element()}].

-spec from_template(Snippet, Ctx) -> Term when
      Snippet :: xml_snippet(),
      Ctx :: context(),
      Term :: exml:element().
from_template(Snippet, Ctx) ->
    xml_to_xmlterm(iolist_to_binary(render(Snippet, Ctx))).

%%--------------------------------------------------------------------
%% Helpers for stanzas from XML
%%--------------------------------------------------------------------

%% @doc An xml() is a well-formed XML document.
%% No multiple top-level elements are allowed.
-type xml() :: binary().

-spec xml_to_xmlterm(XML) -> Term when
      XML :: xml(),
      Term :: exml:element().
xml_to_xmlterm(XML) when is_binary(XML) ->
    {ok, Term} = exml:parse(XML),
    Term.

-spec render(Snippet, Ctx) -> Text when
      Snippet :: xml_snippet(),
      Ctx :: context(),
      Text :: binary().

render(Snippet, Ctx) ->
    ValidateCtx = validate_context(Ctx),
    bbmustache:render(xml_snippet_to_binary(Snippet),
                      ValidateCtx).

xml_snippet_to_binary(Snippet) when is_binary(Snippet) -> Snippet;
xml_snippet_to_binary(Snippet) -> ?io2b(Snippet).

validate_context(Ctx) ->
    [ {atom_to_list(Key), argument_to_string(Value)} || {Key, Value} <- Ctx ].

argument_to_string({Name, Value}) ->
    ?b2l(?io2b([Name, "=\"", Value, "\""]));
argument_to_string(E = #xmlel{}) ->
    ?b2l(?io2b(exml:to_iolist(E)));
argument_to_string(E) when is_binary(E) -> ?b2l(E);
argument_to_string(E) when is_list(E) -> E;
argument_to_string(I) when is_integer(I) -> ?i2l(I);
argument_to_string(F) when is_float(F) -> io_lib:format("~.2f", [F]).
