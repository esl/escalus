-module(user_db_module_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(a(Condition), ?assert(Condition)).
-define(eq(A, B), ?assertEqual(A, B)).

-define(WHEN(Block), begin Block end).
-define(THEN(Block), begin Block end).


all() ->
    [user_db_module_is_started_and_stopped_test].

user_db_module_is_started_and_stopped_test(Config) ->
    C1 = given_escalus_user_db_module(Config, testing_user_db),
    meck:expect(testing_user_db, start, fun(_) -> ok end),
    meck:expect(testing_user_db, stop, fun(_) -> ok end),

    ?WHEN(escalus_starts_and_stops(C1)),

    ?THEN(begin
              ?eq(1, meck:num_calls(testing_user_db, start, '_')),
              ?eq(1, meck:num_calls(testing_user_db, stop, '_'))
          end).

given_escalus_user_db_module(Config, ModuleName) ->
    catch(meck:unload(ModuleName)),
    meck:new(ModuleName, [no_link, non_strict]),
    [{escalus_user_db, {module, ModuleName}}|Config].

escalus_starts_and_stops(C1) ->
    escalus:init_per_suite(C1),
    escalus:end_per_suite(C1).
