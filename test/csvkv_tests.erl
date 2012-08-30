%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Tests for csvkv utilities
%%% @end
%%%===================================================================

-module(csvkv_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

parse_test() ->
    ?assertEqual([], csvkv:parse(<<"">>)),
    ?assertEqual([{<<"key">>, <<"value">>}],
                 csvkv:parse(<<"key=value">>)),
    ?assertEqual([{<<"key">>, <<"value">>}],
                 csvkv:parse(<<"key=\"value\"">>)),
    ?assertEqual([{<<"key">>, <<"value">>},
                  {<<"a">>, <<"b">>}],
                 csvkv:parse(<<"key=value,a=b">>)),
    ?assertEqual([{<<"key">>, <<"value">>},
                  {<<"a">>, <<"b">>}],
                 csvkv:parse(<<"key=\"value\",a=b">>)),
    ?assertEqual([{<<"a">>, <<"b">>},
                  {<<"c">>, <<"d">>},
                  {<<"e">>, <<"f">>}],
                 csvkv:parse(<<"a=b,c=\"d\",e=f">>)),
    ?assertEqual([{<<"a">>, <<"b">>},
                  {<<"c">>, <<"some \"funny\" key">>},
                  {<<"e">>, <<"f">>}],
                 csvkv:parse(<<"a=b,c=\"some \\\"funny\\\" key\",e=f">>)),
    ?assertEqual([{<<"a">>, <<"b">>},
                  {<<"c">>, <<"more \"fun\"">>},
                  {<<"e">>, <<"f">>}],
                 csvkv:parse(<<"a=b,c=\"more \\\"fun\\\"\",e=f">>)),
    ?assertEqual([{<<"a">>, <<"b">>},
                  {<<"c">>, <<"\"fun\"">>},
                  {<<"e">>, <<"f">>}],
                 csvkv:parse(<<"a=b,c=\"\\\"fun\\\"\",e=f">>)),
    ?assertEqual([{<<"a">>, <<"b">>},
                  {<<"c">>, <<"\"fun\"">>}],
                 csvkv:parse(<<"a=b,c=\"\\\"fun\\\"\"">>)),
    ok.
