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

-module(escalus_users).

% Public API
-export([get_jid/1,
         get_username/1,
         get_users/1,
         get_userspec/2,
         get_user_by_name/1,
         create_user/1,
         verify_creation/1,
         delete_user/1,
         get_usp/1,
         make_everyone_friends/1]).

-include("include/escalus.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

get_jid(Name) ->
    {Name, Spec} = get_user_by_name(Name),
    [U, S, _] = get_usp(Spec),
    U ++ "@" ++ S.

get_username(Name) ->
    {Name, Spec} = get_user_by_name(Name),
    [U, _, _] = get_usp(Spec),
    U.

get_users(all) ->
    ct:get_config(escalus_users);
get_users({by_name, Names}) ->
    All = get_users(all),
    [get_user_by_name(Name, All) || Name <- Names];
get_users(Users) ->
    Users.

get_user_by_name(Name) ->
    get_user_by_name(Name, get_users(all)).

get_userspec(Config, Username) ->
    {escalus_users, Users} = proplists:lookup(escalus_users, Config),
    {Username, UserSpec} = proplists:lookup(Username, Users),
    UserSpec.

create_user({_Name, UserSpec}) ->
    Session = escalus_client:start_session([], UserSpec, random),
    GetFields = exmpp_client_register:get_registration_fields(),
    exmpp_session:send_packet(Session, GetFields),
    {ok, result, RegisterInstrs} = wait_for_result("create user"),
    Id = exmpp_stanza:get_id(GetFields),
    FieldKeys = get_registration_questions(RegisterInstrs),
    Fields = [Field || Key <- FieldKeys,
                       none /= (Field = proplists:lookup(Key, UserSpec))],
    Register = exmpp_client_register:register_account(Id, Fields),
    exmpp_session:send_packet(Session, Register),
    Result = wait_for_result("create user"),
    exmpp_session:stop(Session),
    Result.

verify_creation({ok, result, _}) ->
    ok;
verify_creation({ok, conflict, Raw}) ->
    RawStr = exmpp_xml:document_to_iolist(Raw),
    error_logger:info_msg("user already existed: ~s~n", [RawStr]);
verify_creation({error, Error, Raw}) ->
    RawStr = exmpp_xml:document_to_iolist(Raw),
    error_logger:error_msg("error when trying to register user: ~s~n", [RawStr]),
    exit(Error).

delete_user({_Name, UserSpec}) ->
    Session = escalus_client:start_session([], UserSpec, random),
    {ok, _JID} = escalus_client:login([], Session, UserSpec),
    Packet = exmpp_client_register:remove_account(),
    exmpp_session:send_packet(Session, Packet),
    Result = wait_for_result("delete user"),
    exmpp_session:stop(Session),
    Result.

get_usp(UserSpec) ->
    {username, Username} = proplists:lookup(username, UserSpec),
    {server, Server} = proplists:lookup(server, UserSpec),
    {password, Password} = proplists:lookup(password, UserSpec),
    [Username, Server, Password].

make_everyone_friends(Users) ->
    % start the clients
    Config = escalus_cleaner:start([]),
    Clients = start_clients(Config, Users, "friendly"),

    SACs = [{S, [C]} || {S, C} <- lists:zip(Users, Clients)],
    escalus_story:drop_initial_stanzas(Config, SACs, length(Clients)),

    % exchange subscribe and subscribed stanzas
    escalus_utils:exchange_stanzas(Clients, exmpp_presence:subscribe(), short_jid),
    escalus_utils:exchange_stanzas(Clients, exmpp_presence:subscribed(), short_jid),

    % stop the clients
    escalus_cleaner:stop(Config).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_user_by_name(Name, Users) ->
    {Name, _} = proplists:lookup(Name, Users).

wait_for_result(_Action) ->
    receive
        #received_packet{packet_type=iq, type_attr="result", raw_packet=Raw} ->
            % RawStr = exmpp_xml:document_to_iolist(Raw),
            % error_logger:info_msg("success when trying to ~s: ~s~n", [Action, RawStr]),
            {ok, result, Raw};
        #received_packet{packet_type=iq, type_attr="error", raw_packet=Raw} ->
            case is_conflict_stanza(Raw) of
                true ->
                    {ok, conflict, Raw};
                false ->
                    {error, failed_to_register, Raw}
            end
        after 3000 ->
            {error, timeout, exmpp_xml:cdata("timeout")}
    end.

is_conflict_stanza(Stanza) ->
    exmpp_xml:get_attribute(Stanza, <<"type">>, x) == <<"error">> andalso
    exmpp_xml:get_path(Stanza, [{element, "error"}, {attribute, <<"code">>}]) == <<"409">>.

get_registration_questions(Stanza) ->
    Query = exmpp_xml:get_element(Stanza, "query"),
    Children = exmpp_xml:get_child_elements(Query),
    ChildrenNames = lists:map(fun exmpp_xml:get_name_as_atom/1, Children),
    ChildrenNames -- [instructions].

-spec start_clients(list(), list(), string()) -> [#client{}].
start_clients(Config, Users, Presence) ->
    [escalus_client:start(Config, UserSpec, Presence) || {_Name, UserSpec} <- Users].
