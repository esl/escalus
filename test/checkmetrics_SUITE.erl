%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2017 16:55
%%%-------------------------------------------------------------------
-module(checkmetrics_SUITE).
-author("bartek").
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
all() ->
    [ basic_pass, basic_fail, scope ].

basic_pass(_) ->
    pass(5, {1, 6}),
    pass(changed, {1, 6}),
    pass([{n, '>'}], {[{n, 1}], [{n, 2}]}),
    pass([{n, '=='}], {[{n, 1}], [{n, 1}]}),
    pass([{n, 3}], {[{n, 1}], [{n, 4}]}),
    ok.

basic_fail(_) ->
    fail(5, {1, 7}),
    fail(changed, {1, 1}),
    fail([{n, '>'}], {[{n, 1}], [{n, 1}]}),
    fail([{n, '=='}], {[{n, 1}], [{n, 2}]}),
    fail([{n, 3}], {[{n, 1}], [{n, 7}]}),
    ok.

scope(_) ->
    pass({4, 6}, {1, 5}),
    pass({4, 6}, {1, 6}),
    pass({4, 6}, {1, 7}),
    fail({4, 6}, {1, 4}),
    fail({4, 6}, {1, 8}),
    ok.

pass(Change, {Before, After}) ->
    Res = escalus_mongooseim:check_metric_change({{a_metric, Change}, Before, After}, []),
    ?assertEqual([], Res).

fail(Change, {Before, After}) ->
    Res = escalus_mongooseim:check_metric_change({{a_metric, Change}, Before, After}, []),
    ?assertNotEqual([], Res).
