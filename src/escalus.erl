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

-module(escalus).

-export_type([config/0]).

%%--------------------------------------------------------------------
%% Public Types
%%--------------------------------------------------------------------

-type config() :: escalus_config:config().

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

suite() ->
    [{require, escalus_users}].

init_per_suite(Config) ->
    ensure_started(escalus),
    escalus_users:start(Config),
    Config.

end_per_suite(Config) ->
    escalus_users:stop(Config),
    ok.

init_per_testcase(CaseName, Config) ->
    Config1 = escalus_cleaner:start(Config),
    escalus_event:start([{tc_name, CaseName}|Config1]).

end_per_testcase(_CaseName, Config) ->
    Config1 = escalus_event:stop(Config),
    escalus_cleaner:stop(Config1).

%%--------------------------------------------------------------------
%% Public API - forward functions from other modules
%%--------------------------------------------------------------------

-define(FORWARD1(M, F), F(X) -> M:F(X)).
-define(FORWARD2(M, F), F(X, Y) -> M:F(X, Y)).
-define(FORWARD3(M, F), F(X, Y, Z) -> M:F(X, Y, Z)).

?FORWARD1(escalus_users, create_users).
?FORWARD2(escalus_users, create_users).
?FORWARD1(escalus_users, delete_users).
?FORWARD2(escalus_users, delete_users).

?FORWARD1(escalus_story, make_everyone_friends).
?FORWARD3(escalus_story, story).

?FORWARD2(escalus_new_assert, assert).
?FORWARD3(escalus_new_assert, assert).
?FORWARD2(escalus_new_assert, assert_many).

?FORWARD2(escalus_client, send).
?FORWARD2(escalus_client, send_and_wait).
?FORWARD1(escalus_client, wait_for_stanza).
?FORWARD2(escalus_client, wait_for_stanza).
?FORWARD2(escalus_client, wait_for_stanzas).
?FORWARD3(escalus_client, wait_for_stanzas).
?FORWARD1(escalus_client, peek_stanzas).

?FORWARD3(escalus_overridables, override).

ensure_started(App) ->
    case application:start(App) of
        {error, {not_started, NotStartedApp}} ->
            ensure_started(NotStartedApp),
            ensure_started(App);
        ok ->
            ok;
        {error, {already_started, _}} ->
            ok
    end.