-module(regressions_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(a(Condition), ?assert(Condition)).
-define(eq(A, B), ?assertEqual(A, B)).

-import(escalus_story, [story/3]).

all() ->
    [catch_timeout_when_waiting_for_stanza,
     catch_escalus_compat_bin_badarg].

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
                  {'EXIT', ErrorReason} = (catch escalus:wait_for_stanza(Alice)),
                  ?a(is_2_tuple(ErrorReason)),
                  ?eq(timeout_when_waiting_for_stanza, element(1, ErrorReason))
          end).

catch_escalus_compat_bin_badarg(_) ->
    {'EXIT', ErrorReason} = (catch escalus_compat:bin({ala, <<"ma">>, {k,o,t,a}})),
    ?a(is_2_tuple(ErrorReason)),
    ?eq(badarg, element(1, ErrorReason)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

is_2_tuple(T) when is_tuple(T), tuple_size(T) == 2 -> true;
is_2_tuple(_)                                      -> false.
