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

-behaviour(escalus_user_db).

%% `escalus_user_db` callbacks
-export([start/1,
         stop/1,
         create_users/2,
         delete_users/2]).

%% Public API
-export([create_users/1,
         delete_users/1,
         get_jid/2,
         get_username/2,
         get_host/2,
         get_server/2,
         get_userspec/2,
         update_userspec/4,
         get_options/2,
         get_options/3,
         get_options/4,
         get_users/1,
         get_user_by_name/1,
         create_user/2,
         verify_creation/1,
         delete_user/2,
         get_usp/2,
         is_mod_register_enabled/1
        ]).

%% Deprecated API
-export([create_user/1,
         delete_user/1,
         get_usp/1,
         get_jid/1,
         get_username/1,
         get_server/1]).

%% Public types
-export_type([spec/0,
              who/0]).

%% Public types
-type spec() :: [{escalus_config:key(), any()}].
-type who() :: all | {by_name, [escalus_config:key()]} | [spec()].

-import(escalus_compat, [bin/1]).

-include("include/escalus.hrl").
-include_lib("exml/include/exml.hrl").

%%--------------------------------------------------------------------
%% `escalus_user_db` callbacks
%%--------------------------------------------------------------------

start(Config) ->
    case auth_type(Config) of
        {escalus_user_db, {module, M}} ->
            M:start([]);
        _ ->
            ok
    end.

stop(Config) ->
    case auth_type(Config) of
        {escalus_user_db, {module, M}} ->
            M:stop([]);
        _ ->
            ok
    end.

-spec create_users(escalus:config(), who()) -> escalus:config().
create_users(Config, Who) ->
    Users = get_users(Who),
    case auth_type(Config) of
        {escalus_user_db, xmpp} ->
            CreationResults = [create_user(Config, User) || User <- Users],
            lists:foreach(fun verify_creation/1, CreationResults);
        {escalus_user_db, {module, M}} ->
            M:create_users(Config, Users);
        {escalus_user_db, ejabberd} ->
            escalus_ejabberd:create_users(Config, Users)
    end,
    [{escalus_users, Users}] ++ Config.

-spec delete_users(escalus:config(), who()) -> escalus:config().
delete_users(Config, Who) ->
    Users = case Who of
        config -> escalus_config:get_config(escalus_users, Config, []);
        _      -> get_users(Who)
    end,
    case auth_type(Config) of
        {escalus_user_db, xmpp} ->
            [delete_user(Config, User) || User <- Users];
        {escalus_user_db, {module, M}} ->
            M:delete_users(Config, Users);
        {escalus_user_db, ejabberd} ->
            escalus_ejabberd:delete_users(Config, Users)
    end.

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

-spec create_users(escalus:config()) -> escalus:config().
create_users(Config) ->
    create_users(Config, all).

-spec delete_users(escalus:config()) -> escalus:config().
delete_users(Config) ->
    delete_users(Config, all).

get_jid(Config, User) ->
    Username = get_username(Config, User),
    Server = get_server(Config, User),
    <<Username/binary, "@", Server/binary>>.

get_username(Config, User) ->
    get_defined_option(Config, User, username, escalus_username).

get_password(Config, User) ->
    get_defined_option(Config, User, password, escalus_password).

get_host(Config, User) ->
    get_user_option(host, User, escalus_host, Config, get_server(Config, User)).

get_port(Config, User) ->
    get_user_option(port, User, escalus_port, Config, 5222).

get_server(Config, User) ->
    get_user_option(server, User, escalus_server, Config, <<"localhost">>).

get_wspath(Config, User) ->
    get_user_option(wspath, User, escalus_wspath, Config, undefined).

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
get_auth_method(<<"SCRAM-SHA-1">>) ->
    {escalus_auth, auth_sasl_scram_sha1};
get_auth_method(Other) ->
    Other.

get_usp(Config, User) ->
    [get_username(Config, User),
     get_server(Config, User),
     get_password(Config, User)].

%% TODO: get_options/2 and get_userspec/2 are redundant - remove one
%% TODO: this list of options should be complete and formal!
get_options(Config, User) ->
    [{username, get_username(Config, User)},
     {server, get_server(Config, User)},
     {host, get_host(Config, User)},
     {port, get_port(Config, User)},
     {auth, get_auth_method(Config, User)},
     {wspath, get_wspath(Config, User)}
     | get_userspec(Config, User)].

get_options(Config, User, Resource) ->
    [{resource, bin(Resource)} | get_options(Config, User)].

get_options(Config, User, Resource, EventClient) ->
    [{event_client, EventClient} | get_options(Config, User, Resource)].

-spec get_userspec(escalus:config(), escalus_config:key() | spec())
    -> spec().
get_userspec(Config, Username) when is_atom(Username) ->
    Users = escalus_config:get_config(escalus_users, Config),
    {Username, UserSpec} = lists:keyfind(Username, 1, Users),
    UserSpec;
get_userspec(_Config, UserSpec) when is_list(UserSpec) ->
    UserSpec.

update_userspec(Config, UserName, Option, Value) ->
    UserSpec = [{Option, Value}
                | escalus_users:get_userspec(Config, UserName)],
    Users = escalus_config:get_config(escalus_users, Config),
    NewUsers = lists:keystore(UserName, 1, Users, {UserName, UserSpec}),
    lists:keystore(escalus_users, 1, Config, {escalus_users, NewUsers}).

-spec get_users(who()) -> [spec()].
get_users(all) ->
    escalus_ct:get_config(escalus_users);
get_users({by_name, Names}) ->
    All = get_users(all),
    [get_user_by_name(Name, All) || Name <- Names];
get_users(Users) ->
    Users.

get_user_by_name(Name) ->
    get_user_by_name(Name, get_users(all)).

-spec create_user(escalus:config(),
                  {atom(), escalus_config:key() | escalus:config()}) ->
    {ok, any(), xmlterm()} | {error, any(), xmlterm()}.
create_user(Config, {_Name, UserSpec}) ->
    ClientProps = get_options(Config, UserSpec),
    {ok, Conn, ClientProps, _} = escalus_connection:start(ClientProps,
                                                          [start_stream,
                                                           stream_features,
                                                           maybe_use_ssl]),
    escalus_connection:send(Conn, escalus_stanza:get_registration_fields()),
    {ok, result, RegisterInstrs} = wait_for_result(Conn),
    Answers = get_answers(ClientProps, RegisterInstrs),
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
    error(Error).

delete_user(Config, {_Name, UserSpec}) ->
    Options = get_options(Config, UserSpec),
    {ok, Conn, _, _} = escalus_connection:start(Options),
    escalus_connection:send(Conn, escalus_stanza:remove_account()),
    Result = wait_for_result(Conn),
    escalus_connection:stop(Conn),
    Result.

auth_type(Config) ->
    Type = case {escalus_config:get_config(escalus_user_db, Config, undefined),
                 try_check_mod_register(Config)} of
               {{module, _} = M, _} -> M;
               {_, true} -> xmpp;
               {_, false} -> ejabberd
           end,
    {escalus_user_db, Type}.

try_check_mod_register(Config) ->
    try is_mod_register_enabled(Config)
    catch _ -> false
    end.

is_mod_register_enabled(Config) ->
    Server = escalus_config:get_config(escalus_server, Config, <<"localhost">>),
    Host = escalus_config:get_config(escalus_host, Config, Server),
    Port = escalus_config:get_config(escalus_port, Config, 5222),
    ClientProps = [{server, Server},
                   {host, Host},
                   {port, Port}],
    {ok, Conn, ClientProps, _} = escalus_connection:start(ClientProps,
                                                          [start_stream,
                                                           stream_features,
                                                           maybe_use_ssl]),
    escalus_connection:send(Conn, escalus_stanza:get_registration_fields()),
    case wait_for_result(Conn) of
        {error, _, _} ->
            false;
        _ ->
            true
    end.

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

-spec get_user_by_name(atom(), escalus:config()) -> {atom(), escalus:config()}.
get_user_by_name(Name, Users) ->
    {Name, _} = proplists:lookup(Name, Users).

-type short_option() :: 'username'    %% binary()
                      | 'server'      %% binary()
                      | 'password'    %% binary()
                      | 'compression' %% <<"zlib">> | false
                      | 'ssl'         %% 'false' | 'optional',
                                      %% shouldn't there also be 'required'?
                      | 'transport'   %% 'tcp' | 'bosh' | 'ws', anything else?
                      | 'path'        %% BOSH path
                      | 'port'        %% TCP port
                      | 'wspath'      %% WebSocket path - unify with `path`?
                      | 'host'        %% IP address? DNS name?
                      | 'auth_method' %% <<"PLAIN">> | <<"DIGETS-MD5">>
                                      %% | <<"SASL-ANON">> | <<"SCRAM-SHA-1">>
                                      %% | Other
                      .

-type ejabberd_option() :: 'ejabberd_node'
                         | 'ejabberd_cookie'
                         | 'ejabberd_domain'.

-type escalus_option() :: 'escalus_server'
                        | 'escalus_username'
                        | 'escalus_password'
                        | 'escalus_host'
                        | 'escalus_port'
                        | 'escalus_auth_method'
                        | 'escalus_wspath'
                        .

-type long_option() :: ejabberd_option() | escalus_option().

-type option_value() :: any().

%% get_user_option is a wrapper on escalus_config:get_config/5,
%% which can take either UserSpec (a proplist) or user name (atom)
%% as the second argument
-spec get_user_option(short_option(), atom() | escalus:config(), long_option(),
                      escalus:config(), option_value()) -> option_value().
get_user_option(Short, Name, Long, Config, Default) when is_atom(Name) ->
    {Name, Spec} = case lists:keysearch(escalus_users, 1, Config) of
        false ->
            get_user_by_name(Name);
        {value, {_, Users}} ->
            get_user_by_name(Name, Users)
    end,
    get_user_option(Short, Spec, Long, Config, Default);
get_user_option(Short, Spec, Long, Config, Default) ->
    escalus_config:get_config(Short, Spec, Long, Config, Default).

-spec get_defined_option(escalus:config(), atom() | escalus:config(),
                         short_option(), long_option()) -> option_value().
get_defined_option(Config, Name, Short, Long) ->
    case get_user_option(Short, Name, Long, Config, undefined) of
        undefined ->
            escalus_ct:fail({undefined_option, Short, Name});
        Value ->
            Value
    end.

-spec wait_for_result(escalus:client()) -> {ok, result, xmlterm()}
                                         | {ok, conflict, xmlterm()}
                                         | {error, Error, xmlterm()}
      when Error :: 'failed_to_register' | 'bad_response' | 'timeout'.
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

response_type(#xmlel{name = <<"iq">>} = IQ) ->
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
    ChildrenNames = [N || #xmlel{name = N} <- Query#xmlel.children],
    NoInstr = ChildrenNames -- [<<"instructions">>],
    [#xmlel{name=K, children=[exml:escape_cdata(proplists:get_value(K, BinSpec))]}
     || K <- NoInstr].
