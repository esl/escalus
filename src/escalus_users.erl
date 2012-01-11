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
-export([create_users/1,
         create_users/2,
         delete_users/1,
         get_jid/1,
         get_username/1,
         get_server/1,
         get_users/1,
         get_userspec/2,
         get_user_by_name/1,
         create_user/1,
         verify_creation/1,
         delete_user/1,
         get_usp/1]).

-import(escalus_compat, [unimplemented/0]).

-include("include/escalus.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("lxmppc/include/lxmppc.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

create_users(Config) ->
    create_users(Config, all).

create_users(Config, Who) ->
    Users = get_users(Who),
    CreationResults = lists:map(fun create_user/1, Users),
    lists:foreach(fun verify_creation/1, CreationResults),
    [{escalus_users, Users}] ++ Config.

delete_users(Config) ->
    {escalus_users, Users} = proplists:lookup(escalus_users, Config),
    lists:foreach(fun delete_user/1, Users).

get_jid(Name) ->
    {Name, Spec} = get_user_by_name(Name),
    [U, S, _] = get_usp(Spec),
    <<U/binary,"@",S/binary>>.

get_username(Name) ->
    {Name, Spec} = get_user_by_name(Name),
    [U, _, _] = get_usp(Spec),
    U.

get_server(Name) ->
    {Name, Spec} = get_user_by_name(Name),
    [_, S, _] = get_usp(Spec),
    S.

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
    {ok, Conn, _} = lxmppc:connect(UserSpec),
    lxmppc_session:start_stream(Conn, []),
    lxmppc:send(Conn, escalus_stanza:get_registration_fields()),
    {ok, result, RegisterInstrs} = wait_for_result(Conn),
    Answers = get_answers(UserSpec, RegisterInstrs),
    lxmppc:send(Conn, escalus_stanza:register_account(Answers)),
    Result = wait_for_result(Conn),
    lxmppc:stop(Conn),
    Result.

verify_creation({ok, result, _}) ->
    ok;
verify_creation({ok, conflict, Raw}) ->
    RawStr = exml:to_iolist(Raw),
    error_logger:info_msg("user already existed: ~s~n", [RawStr]);
verify_creation({error, Error, Raw}) ->
    RawStr = exml:to_iolist(Raw),
    error_logger:error_msg("error when trying to register user: ~s~n", [RawStr]),
    exit(Error).

delete_user({_Name, UserSpec}) ->
    {ok, Conn, _} = lxmppc:start(UserSpec),
    lxmppc:send(Conn, escalus_stanza:remove_account()),
    Result = wait_for_result(Conn),
    lxmppc:stop(Conn),
    Result.

get_usp(UserSpec) ->
    {username, Username} = proplists:lookup(username, UserSpec),
    {server, Server} = proplists:lookup(server, UserSpec),
    {password, Password} = proplists:lookup(password, UserSpec),
    [Username, Server, Password].

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_user_by_name(Name, Users) ->
    {Name, _} = proplists:lookup(Name, Users).

wait_for_result(Conn) ->
    receive
        {stanza, Conn, Stanza} ->
            case response_type(Stanza) of
                result ->
                    {ok, result, Stanza};
                conflict ->
                    {ok, conflict, Stanza};
                error ->
                    {error, failed_to_register, Stanza};
                _ ->
                    {error, bad_response, Stanza}
            end
    after 3000 ->
            {error, timeout, exml:escape_cdata(<<"timeout">>)}
    end.

response_type(#xmlelement{name = <<"iq">>} = IQ) ->
    case exml_query:attr(IQ, <<"type">>) of
        <<"result">> ->
            result;
        <<"error">> ->
            case exml_query:path(IQ, [{element, <<"error">>},
                                      {attr, <<"code">>}]) of
                <<"409">> ->
                    conflict;
                _ ->
                    error
            end;
        _ ->
            other
    end;
response_type(_) ->
    other.

get_answers(UserSpec, InstrStanza) ->
    BinSpec = [{list_to_binary(atom_to_list(K)), V} || {K, V} <- UserSpec],
    Query = exml_query:subelement(InstrStanza, <<"query">>),
    ChildrenNames = [N || #xmlelement{name = N} <- Query#xmlelement.body],
    NoInstr = ChildrenNames -- [<<"instructions">>],
    [#xmlelement{name=K, body=[exml:escape_cdata(proplists:get_value(K, BinSpec))]}
     || K <- NoInstr].
