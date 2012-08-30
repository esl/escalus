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

%% Note: This module requires ejabberd_node and ejabberd_cookie
%%       to be set if common test configuration file

-module(escalus_ejabberd).

-export([rpc/3,
         remote_display/1,
         remote_format/1,
         remote_format/2,
         create_users/1,
         create_users/2,
         delete_users/1,
         get_global_option/1,
         with_global_option/3,
         with_local_option/3,
         get_c2s_status/1,
         get_remote_sessions/1,
         default_get_remote_sessions/0,
         wait_for_session_count/2]).

-include("include/escalus.hrl").

rpc(M, F, A) ->
    Node = ct:get_config(ejabberd_node),
    Cookie = ct:get_config(ejabberd_cookie),
    ct_rpc:call(Node, M, F, A, 3000, Cookie).

remote_display(String) ->
    Line = [$\n, [$- || _ <- String], $\n],
    remote_format("~s~s~s", [Line, String, Line]).

remote_format(Format) ->
    remote_format(Format, []).

remote_format(Format, Args) ->
    group_leader(rpc(erlang, whereis, [user]), self()),
    io:format(Format, Args),
    group_leader(whereis(user), self()).

% API - compatible with escalus_users:create_users/1
create_users(Config) ->
    create_users(Config, all).

% API - compatible with escalus_users:create_users/2
create_users(Config, Who) ->
    AllUsers = escalus_users:get_users(Who),
    lists:foreach(fun({_Name, UserSpec}) ->
        register_user(Config, UserSpec)
    end, AllUsers),
    Config.

% API - compatible with escalus_users:delete_users/1
delete_users(Config) ->
    AllUsers = escalus_config:get_config(escalus_users, Config),
    lists:foreach(fun({_Name, UserSpec}) ->
        unregister_user(Config, UserSpec)
    end, AllUsers).

-spec get_global_option(term()) -> term().
get_global_option(Option) ->
    rpc(ejabberd_config, get_global_option, [Option]).

-spec with_global_option(atom(), any(), fun(() -> Type)) -> Type.
with_global_option(Option, Value, Fun) ->
    OldValue = rpc(ejabberd_config, get_global_option, [Option]),
    rpc(ejabberd_config, add_global_option, [Option, Value]),
    try
        Fun()
    after
        rpc(ejabberd_config, add_global_option, [Option, OldValue])
    end.

-spec with_local_option(atom(), any(), fun(() -> Type)) -> Type.
with_local_option(Option, Value, Fun) ->
    Hosts = rpc(ejabberd_config, get_global_option, [hosts]),
    OldValues = lists:map(fun(Host) ->
        OldValue = rpc(mnesia, dirty_read, [local_config, {Option, Host}]),
        rpc(ejabberd_config, add_local_option, [{Option, Host}, Value]),
        OldValue
    end, Hosts),
    try
        Fun()
    after
        lists:foreach(
            fun ({Host, [{local_config, {Host, _Opt}, OldValue}]}) ->
                    rpc(ejabberd_config, add_local_option, [{Option, Host}, OldValue]);
                ({Host, []}) ->
                    rpc(mnesia, dirty_delete, [local_config, {Option, Host}])
        end, lists:zip(Hosts, OldValues))
    end.

get_c2s_status(#client{jid=Jid}) ->
    {match, USR} = re:run(Jid, <<"([^@]*)@([^/]*)/(.*)">>, [{capture, all_but_first, list}]),
    Pid = rpc(ejabberd_sm, get_session_pid, USR),
    rpc(sys, get_status, [Pid]).

wait_for_session_count(Config, Count) ->
    wait_for_session_count(Config, Count, 0).

get_remote_sessions(Config) ->
    escalus_overridables:do(Config, get_remote_sessions, [],
                            {?MODULE, default_get_remote_sessions}).

wait_for_session_count(Config, Count, TryNo) when TryNo < 20 ->
    case length(get_remote_sessions(Config)) of
        Count ->
            ok;
        _ ->
            timer:sleep(TryNo * TryNo),
            wait_for_session_count(Config, Count, TryNo + 1)
    end;
wait_for_session_count(Config, Count, _) ->
    ct:fail({wait_for_session_count, Count, get_remote_sessions(Config)}).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

register_user(Config, UserSpec) ->
    rpc(ejabberd_admin, register, escalus_users:get_usp(Config, UserSpec)).

unregister_user(Config, UserSpec) ->
    [U, S, _P] = escalus_users:get_usp(Config, UserSpec),
    rpc(ejabberd_admin, unregister, [U, S]).

default_get_remote_sessions() ->
    rpc(ejabberd_sm, get_full_session_list, []).
