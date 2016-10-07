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

% Public API
-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         create_users/1,
         create_users/2,
         delete_users/1,
         delete_users/2,
         get_users/1,
         override/3,
         make_everyone_friends/1,
         fresh_story/3,
         fresh_story_with_config/3,
         story/3,
         assert/2,
         assert/3,
         assert_many/2,
         send/2,
         send_and_wait/2,
         wait_for_stanza/1,
         wait_for_stanza/2,
         wait_for_stanzas/2,
         wait_for_stanzas/3,
         peek_stanzas/1]).

-export_type([client/0,
              config/0]).

-include("escalus.hrl").

%%--------------------------------------------------------------------
%% Public Types
%%--------------------------------------------------------------------

-type client() :: #client{}.
-type config() :: escalus_config:config().

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------
-spec suite() -> [{atom(), atom()}].
suite() ->
    [{require, escalus_users}].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    ensure_started(escalus),
    escalus_users:start(Config),
    escalus_fresh:start(Config),
    Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
    escalus_users:stop(Config),
    escalus_fresh:stop(Config),
    ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(CaseName, Config) ->
    Config1 = escalus_cleaner:start(Config),
    escalus_event:start([{tc_name, CaseName}|Config1]).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_CaseName, Config) ->
    Config1 = escalus_event:stop(Config),
    escalus_cleaner:stop(Config1).

%%--------------------------------------------------------------------
%% Public API - forward functions from other modules
%%--------------------------------------------------------------------

%% User API
-spec create_users(config()) -> config().
create_users(Config) ->
    escalus_users:create_users(Config).

-spec create_users(config(), escalus_users:user_spec()) -> config().
create_users(Config, Users) ->
    escalus_users:create_users(Config, Users).

-spec delete_users(config()) -> config().
delete_users(Config) ->
    escalus_users:delete_users(Config).

-spec delete_users(config(), escalus_users:user_spec()) -> config().
delete_users(Config, Users) ->
    escalus_users:delete_users(Config, Users).

-spec get_users(Spec) -> Result when
      Spec :: all | [escalus_users:user_name()]
              | {by_name, [escalus_users:user_name()]},
      Result :: [escalus_users:named_user()].
get_users(Names) ->
    escalus_users:get_users(Names).

%% Story API

-spec make_everyone_friends(config()) -> config().
make_everyone_friends(Config) ->
    escalus_story:make_everyone_friends(Config).

-spec fresh_story(config(), escalus_users:user_spec(), fun()) -> any().
fresh_story(Config, ResourceCounts, Story) ->
    escalus_fresh:story(Config, ResourceCounts, Story).

-spec fresh_story_with_config(config(), escalus_users:user_spec(), fun()) -> any().
fresh_story_with_config(Config, ResourceCounts, Story) ->
    escalus_fresh:story_with_config(Config, ResourceCounts, Story).

-spec story(config(), escalus_users:user_spec(), fun()) -> any().
story(Config, ResourceCounts, Story) ->
    escalus_story:story(Config, ResourceCounts, Story).

%% Assertions

assert(PredSpec, Arg) ->
    escalus_new_assert:assert(PredSpec, Arg).

assert(PredSpec, Params, Arg) ->
    escalus_new_assert:assert(PredSpec, Params, Arg).

assert_many(Predicates, Stanzas) ->
    escalus_new_assert:assert_many(Predicates, Stanzas).

%% Client API

send(Client, Packet) ->
    escalus_client:send(Client, Packet).

send_and_wait(Client, Packet) ->
    escalus_client:send_and_wait(Client, Packet).

wait_for_stanza(Client) ->
    escalus_client:wait_for_stanza(Client).

wait_for_stanza(Client, Timeout) ->
    escalus_client:wait_for_stanza(Client, Timeout).

wait_for_stanzas(Client, Count) ->
    escalus_client:wait_for_stanzas(Client, Count).

wait_for_stanzas(Client, Count, Timeout) ->
    escalus_client:wait_for_stanzas(Client, Count, Timeout).

peek_stanzas(Client) ->
    escalus_client:peek_stanzas(Client).

%% Other functions

override(Config, OverrideName, NewValue) ->
    escalus_overridables:override(Config, OverrideName, NewValue).

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
