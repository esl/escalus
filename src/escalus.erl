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
         story/3]).

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

create_users(Config, NamesSpec) ->
    Users = escalus_users:create_users(NamesSpec),
    [{escalus_users, Users}] ++ Config.

create_users(Config) ->
    create_users(Config, all).

delete_users(Config) ->
    {escalus_users, Users} = proplists:lookup(escalus_users, Config),
    escalus_users:delete_users(Users).

init_per_testcase(_CaseName, Config) ->
    escalus_cleaner:start(Config).

end_per_testcase(_CaseName, Config) ->
    escalus_cleaner:stop(Config).

make_everyone_friends(Config) ->
    {escalus_users, Users} = proplists:lookup(escalus_users, Config),
    escalus_users:make_everyone_friends(Users),
    Config.

story(Config, ResourceCount, Test) ->
    escalus_story:story(Config, ResourceCount, Test).
