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

-behaviour(escalus_user_db).
-export([start/1,
         stop/1,
         create_users/2,
         delete_users/2]).

-behaviour(escalus_server).
-export([pre_story/1,
         post_story/1,
         name/0]).

-export([rpc/3,
         remote_display/1,
         remote_format/1,
         remote_format/2,
         get_global_option/1,
         with_global_option/3,
         with_local_option/3,
         get_c2s_status/1,
         get_remote_sessions/1,
         default_get_remote_sessions/0,
         legacy_get_remote_sessions/0,
         unify_str_arg/1,
         unify_str_arg/2,
         wait_for_session_count/2,
         setup_option/2,
         reset_option/2]).

-include("escalus.hrl").

%%%
%%% Business API
%%%

-spec rpc(atom(), atom(), [any()], timeout()) -> any().
rpc(M, F, A, Timeout) ->
    Node = escalus_ct:get_config(ejabberd_node),
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    escalus_rpc:call(Node, M, F, A, Timeout, Cookie).

-spec rpc(atom(), atom(), [any()]) -> any().
rpc(M, F, A) ->
    rpc(M, F, A, 3000).

-spec remote_display(string()) -> true.
remote_display(String) ->
    Line = [$\n, [$- || _ <- String], $\n],
    remote_format("~s~s~s", [Line, String, Line]).

-spec remote_format(string()) -> true.
remote_format(Format) ->
    remote_format(Format, []).

-spec remote_format(string(), [any()]) -> true.
remote_format(Format, Args) ->
    group_leader(rpc(erlang, whereis, [user]), self()),
    io:format(Format, Args),
    group_leader(whereis(user), self()).

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

-spec get_c2s_status(escalus:client()) -> term().
get_c2s_status(#client{jid = Jid}) ->
    {match, USR} = re:run(Jid, <<"([^@]*)@([^/]*)/(.*)">>, [{capture, all_but_first, list}]),
    Pid = rpc(ejabberd_sm, get_session_pid, USR),
    rpc(sys, get_status, [Pid]).

-spec wait_for_session_count(escalus:config(), non_neg_integer()) -> ok | no_return().
wait_for_session_count(Config, Count) ->
    wait_for_session_count(Config, Count, 0).

-spec get_remote_sessions(escalus:config()) -> term().
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
    escalus_ct:fail({wait_for_session_count,
                     Count, get_remote_sessions(Config)}).

%% Option setup / reset.
%%
%% These two functions are intended for situations where
%% `with_global_option/3` and `with_local_option/3` do not apply
%% because the access pattern to the option is non-uniform
%% (e.g. it's not stored in `config`/`local_config` Mnesia table).
%%
%% Example:
%%
%% - define an `option()` instance in a helper function:
%%
%%     inactivity() ->
%%         {inactivity,
%%          fun() -> escalus_ejabberd:rpc(mod_bosh, get_inactivity, []) end,
%%          fun(Value) -> escalus_ejabberd:rpc(mod_bosh,
%%                                             set_inactivity, [Value]) end,
%%          ?INACTIVITY}.
%%
%%   one can also easily imagine such a constructor to be parameterised:
%%
%%     inactivity(Value) ->
%%         {inactivity,
%%          fun() -> escalus_ejabberd:rpc(mod_bosh, get_inactivity, []) end,
%%          fun(Value) -> escalus_ejabberd:rpc(mod_bosh,
%%                                             set_inactivity, [Value]) end,
%%          Value}.
%%
%% - use `setup_option/2` in `init_per_testcase/2`:
%%
%%     init_per_testcase(disconnect_inactive = CaseName, Config) ->
%%         NewConfig = escalus_ejabberd:setup_option(inactivity(), Config),
%%         escalus:init_per_testcase(CaseName, NewConfig);
%%
%% - use `reset_option/2` in `end_per_testcase/2`:
%%
%%     end_per_testcase(disconnect_inactive = CaseName, Config) ->
%%         NewConfig = escalus_ejabberd:reset_option(inactivity(), Config),
%%         escalus:end_per_testcase(CaseName, NewConfig);
%%
%% Inside `setup_option/2` the current value of `inactivity`,
%% as returned by the RPC call, will be stored in `Config`.
%% The value from `option()` instance will be used
%% to set the new value on the server.
%%
%% `reset_option/2` will restore that saved value using the appropriate
%% setter function and delete it from `Config`.

-type option(Type) :: {Name   :: atom(),
                       Getter :: fun(() -> Type),
                       Setter :: fun((Type) -> any()),
                       Value  :: Type}.
-type option() :: option(_).

-spec setup_option(option(), Config) -> Config.
setup_option({Option, Get, Set, Value}, Config) ->
    Saved = Get(),
    Set(Value),
    [{{saved, Option}, Saved} | Config].

-spec reset_option(option(), Config) -> Config.
reset_option({Option, _, Set, _}, Config) ->
    case proplists:get_value({saved, Option}, Config) of
        undefined ->
            ok;
        Saved ->
            Set(Saved)
    end,
    proplists:delete({saved, Option}, Config).

%%--------------------------------------------------------------------
%% escalus_user_db callbacks
%%--------------------------------------------------------------------

-spec start(_) -> ok.
start(_) ->
    ok.

-spec stop(_) -> ok.
stop(_) ->
    ok.

-spec create_users(escalus:config(), [escalus_users:named_user()]) -> escalus:config().
create_users(Config, Users) ->
    lists:foreach(fun (User) -> register_user(Config, User) end, Users),
    lists:keystore(escalus_users, 1, Config, {escalus_users, Users}).

-spec delete_users(escalus:config(), [escalus_users:named_user()]) -> escalus:config().
delete_users(Config, Users) ->
    lists:foreach(fun(User) -> unregister_user(Config, User) end, Users),
    Config.

%%--------------------------------------------------------------------
%% escalus_server callbacks
%%--------------------------------------------------------------------

-spec pre_story(escalus:config()) -> escalus:config().
pre_story(Config) ->
    Config.

-spec post_story(escalus:config()) -> escalus:config().
post_story(Config) ->
    Config.

-spec name() -> atom().
name() ->
    ?MODULE.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec register_user(escalus:config(), escalus_users:named_user()) -> any().
register_user(Config, {_UserName, UserSpec}) ->
    StrFormat = escalus_ct:get_config(ejabberd_string_format),
    USP = [unify_str_arg(Arg, StrFormat) ||
           Arg <- escalus_users:get_usp(Config, UserSpec)],
    rpc(ejabberd_admin, register, USP).

-spec unregister_user(escalus:config(), escalus_users:named_user()) -> any().
unregister_user(Config, {_UserName, UserSpec}) ->
    StrFormat = escalus_ct:get_config(ejabberd_string_format),
    USP = [unify_str_arg(Arg, StrFormat) ||
           Arg <- escalus_users:get_usp(Config, UserSpec)],
    [U, S, _P] = USP,
    rpc(ejabberd_admin, unregister, [U, S], 30000).

-spec default_get_remote_sessions() -> any().
default_get_remote_sessions() ->
    rpc(ejabberd_sm, get_full_session_list, []).

-spec legacy_get_remote_sessions() -> any().
legacy_get_remote_sessions() ->
    rpc(ejabberd_sm, dirty_get_sessions_list, []).

-spec unify_str_arg(any()) -> any().
unify_str_arg(Arg) ->
    StrFormat = escalus_ct:get_config(ejabberd_string_format),
    unify_str_arg(Arg, StrFormat).

-spec unify_str_arg(any(), str | string()) -> any().
unify_str_arg(Arg, str) when is_binary(Arg) ->
    binary_to_list(Arg);
unify_str_arg(Arg, _) ->
    Arg.

