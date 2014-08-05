-module(regressions_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-import(escalus_story, [story/3]).

all() ->
    [catch_timeout_when_waiting_for_stanza].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

catch_timeout_when_waiting_for_stanza(Config) ->
    story(Config, [{alice,1}],
          fun (Alice) ->
                  Caught = (catch escalus:wait_for_stanza(Alice)),
                  ct:pal("**** caught: ~p", [Caught])
          end).
