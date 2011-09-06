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
         create_users/2,
         create_users/1,
         delete_users/1,
         init_per_testcase/2,
         end_per_testcase/2,
         make_everyone_friends/1,
         story/3,
         assert/2,
         assert/3,
         send/2]).

%%--------------------------------------------------------------------
%% Helper macros
%%--------------------------------------------------------------------

-define(FORWARD1(M, F), F(X) -> M:F(X)).
-define(FORWARD2(M, F), F(X, Y) -> M:F(X, Y)).
-define(FORWARD3(M, F), F(X, Y, Z) -> M:F(X, Y, Z)).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

suite() ->
    [{require, escalus_users}].

init_per_suite(Config) ->
    application:start(exmpp),
    Config.

end_per_suite(_Config) ->
    ok.

create_users(Config, Who) ->
    Users = escalus_users:get_users(Who),
    CreationResults = lists:map(fun escalus_users:create_user/1, Users),
    lists:foreach(fun escalus_users:verify_creation/1, CreationResults),
    [{escalus_users, Users}] ++ Config.

create_users(Config) ->
    create_users(Config, all).

delete_users(Config) ->
    {escalus_users, Users} = proplists:lookup(escalus_users, Config),
    lists:foreach(fun escalus_users:delete_user/1, Users).

init_per_testcase(_CaseName, Config) ->
    escalus_cleaner:start(Config).

end_per_testcase(_CaseName, Config) ->
    escalus_cleaner:stop(Config).

make_everyone_friends(Config) ->
    {escalus_users, Users} = proplists:lookup(escalus_users, Config),
    escalus_story:make_everyone_friends(Config, Users).

?FORWARD3(escalus_story, story).
?FORWARD2(escalus_new_assert, assert).
?FORWARD3(escalus_new_assert, assert).
?FORWARD2(escalus_client, send).
