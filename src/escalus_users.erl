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
         delete_users/2,
         get_jid/2,
         get_username/2,
         get_host/2,
         get_server/2,
         get_userspec/2,
         get_options/2,
         get_options/3,
         get_users/1,
         get_user_by_name/1,
         create_user/2,
         verify_creation/1,
         delete_user/2,
         get_usp/2]).

% deprecated API
-export([create_user/1,
         delete_user/1,
         get_usp/1,
         get_jid/1,
         get_username/1,
         get_server/1]).

-import(escalus_compat, [bin/1]).

-include("include/escalus.hrl").
-include_lib("exml/include/exml.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

create_users(Config) ->
    create_users(Config, all).

create_users(Config, Who) ->
    Users = get_users(Who),
    CreationResults = [create_user(Config, User) || User <- Users],
    lists:foreach(fun verify_creation/1, CreationResults),
    [{escalus_users, Users}] ++ Config.

delete_users(Config) ->
    delete_users(Config, all).

delete_users(Config, Who) ->
    Users = get_users(Who),
    [delete_user(Config, User) || User <- Users].

get_jid(Config, User) ->
    Username = get_username(Config, User),
    Server = get_server(Config, User),
    <<Username/binary, "@", Server/binary>>.

get_username(Config, User) ->
    get_defined_option(Config, User, username, escalus_username).

get_password(Config, User) ->
    get_defined_option(Config, User, password, escalus_password).

get_host(Config, User) ->
    get_user_option(host, User, escalus_host, Config, <<"localhost">>).

get_port(Config, User) ->
    get_user_option(port, User, escalus_port, Config, 5222).

get_server(Config, User) ->
    get_user_option(server, User, escalus_server, Config, get_host(Config, User)).

get_auth_method(Config, User) ->
    AuthMethod = get_user_option(auth_method, User,
                                 escalus_auth_method, Config,
                                 <<"PLAIN">>),
    get_auth_method(AuthMethod).

get_auth_method(<<"PLAIN">>) ->
    {escalus_auth, auth_plain};
get_auth_method(<<"DIGEST-MD5">>) ->
    {escalus_auth, auth_digest_md5};
get_auth_method(<<"SASL-ANON">>) ->
    {escalus_auth, auth_sasl_anon};
get_auth_method(Other) ->
    Other.

get_usp(Config, User) ->
    [get_username(Config, User),
     get_server(Config, User),
     get_password(Config, User)].

get_options(Config, User) ->
    [{username, get_username(Config, User)},
     {server, get_server(Config, User)},
     {host, get_host(Config, User)},
     {port, get_port(Config, User)},
     {auth, get_auth_method(Config, User)}
     | get_userspec(Config, User)].

get_options(Config, User, Resource) ->
    [{resource, bin(Resource)} | get_options(Config, User)].

get_userspec(Config, Username) when is_atom(Username) ->
    Users = escalus_config:get_config(escalus_users, Config),
    {Username, UserSpec} = lists:keyfind(Username, 1, Users),
    UserSpec;
get_userspec(_Config, UserSpec) when is_list(UserSpec) ->
    UserSpec.

%%% XXX: this is so ugly...
get_users(all) ->
    ct:get_config(escalus_users);
get_users({by_name, Names}) ->
    All = get_users(all),
    [get_user_by_name(Name, All) || Name <- Names];
get_users(Users) ->
    Users.

get_user_by_name(Name) ->
    get_user_by_name(Name, get_users(all)).

create_user(Config, {_Name, UserSpec}) ->
    Options0 = get_options(Config, UserSpec),
    {ok, Conn, Options1} = escalus_connection:connect(Options0),
    escalus_session:start_stream(Conn, Options1),
    escalus_connection:send(Conn, escalus_stanza:get_registration_fields()),
    {ok, result, RegisterInstrs} = wait_for_result(Conn),
    Answers = get_answers(Options1, RegisterInstrs),
    escalus_connection:send(Conn, escalus_stanza:register_account(Answers)),
    Result = wait_for_result(Conn),
    escalus_connection:stop(Conn),
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

delete_user(Config, {_Name, UserSpec}) ->
    Options = get_options(Config, UserSpec),
    {ok, Conn, _} = escalus_connection:start(Options),
    escalus_connection:send(Conn, escalus_stanza:remove_account()),
    Result = wait_for_result(Conn),
    escalus_connection:stop(Conn),
    Result.

%%--------------------------------------------------------------------
%% Deprecated API
%%--------------------------------------------------------------------

-define(DEFINE_ONE_ARG(FUNCTION_NAME),
        FUNCTION_NAME(Username) ->
            error_logger:info_msg("Calling deprecated ~p:~p/1,"
                                  " please call a version with Config"
                                  " as additional first argument.~n"
                                  "Backtrace: ~p~n.",
                                  [?MODULE, FUNCTION_NAME,
                                   escalus_compat:backtrace(0)]),
            FUNCTION_NAME([], Username)).

?DEFINE_ONE_ARG(create_user).
?DEFINE_ONE_ARG(delete_user).
?DEFINE_ONE_ARG(get_usp).
?DEFINE_ONE_ARG(get_jid).
?DEFINE_ONE_ARG(get_username).
?DEFINE_ONE_ARG(get_server).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_user_by_name(Name, Users) ->
    {Name, _} = proplists:lookup(Name, Users).

%% get_user_option is a wrapper on escalus_config:get_config/5,
%% which can take either UserSpec (a proplist) or user name (atom)
%% as the second argument
get_user_option(Short, Name, Long, Config, Default) when is_atom(Name) ->
    {Name, Spec} = get_user_by_name(Name),
    get_user_option(Short, Spec, Long, Config, Default);
get_user_option(Short, Spec, Long, Config, Default) ->
    escalus_config:get_config(Short, Spec, Long, Config, Default).

get_defined_option(Config, Name, Short, Long) ->
    case get_user_option(Short, Name, Long, Config, undefined) of
        undefined ->
            ct:fail({undefined_option, Short, Name});
        Value ->
            Value
    end.

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
    ChildrenNames = [N || #xmlelement{name = N} <- Query#xmlelement.children],
    NoInstr = ChildrenNames -- [<<"instructions">>],
    [#xmlelement{name=K, children=[exml:escape_cdata(proplists:get_value(K, BinSpec))]}
     || K <- NoInstr].
