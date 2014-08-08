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
     catch_escalus_compat_bin_badarg,
     %% can't run with current build system, see test for the rationale
     %% catch_assert_false,
     catch_escalus_user_verify_creation].

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
    %% given
    escalus:create_users(Config, {by_name, [alice]}),
    story(Config, [{alice,1}],
          fun (Alice) ->
                  %% when
                  {'EXIT', ErrorReason} = (catch escalus:wait_for_stanza(Alice)),
                  %% then
                  ?a(is_2_tuple(ErrorReason)),
                  ?eq(timeout_when_waiting_for_stanza, element(1, ErrorReason))
          end),
    escalus:delete_users(Config, {by_name, [alice]}).

catch_escalus_compat_bin_badarg(_) ->
    %% when
    {'EXIT', ErrorReason} = (catch escalus_compat:bin({ala, <<"ma">>, {k,o,t,a}})),
    %% then
    ?a(is_2_tuple(ErrorReason)),
    ?eq(badarg, element(1, ErrorReason)).

%% `escalus_new_assert:assert_true/2` is a module-internal function with
%% no public API, so without exporting it just for the sake of test,
%% we can't test it.
%% Exporting just for the sake of tests would be done best with an ifdef(TEST),
%% but then we need the build system to support two types of builds:
%% release and debug. Furthermore, rebar would have to know which to use
%% when running `rebar ct`.
catch_assert_false(_) ->
    %% when
    {'EXIT', ErrorReason} = (catch escalus_new_assert:assert_true(false, my_reason)),
    %% then
    ?a(is_2_tuple(ErrorReason)),
    ?eq(my_reason, element(1, ErrorReason)).

catch_escalus_user_verify_creation(_) ->
    %% given
    {M, F} = {escalus_users, verify_creation},
    RawXMPPError = escalus_stanza:error_element(<<"fake-type">>,
                                                <<"fake-condition">>),
    %% when
    {'EXIT', ErrorReason} = (catch M:F({error, my_error, RawXMPPError})),
    %% then
    ?a(is_2_tuple(ErrorReason)),
    ?eq(my_error, element(1, ErrorReason)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

is_2_tuple(T) when is_tuple(T), tuple_size(T) == 2 -> true;
is_2_tuple(_)                                      -> false.
