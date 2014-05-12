-module(stream_automation_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, automatic}].

groups() ->
    [{automatic, [shuffle, {repeat, 1}], [simple_ack, ack_after_four_messages]},
     {manual, [shuffle, {repeat, 1}], [no_ack]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus_ejabberd:rpc(mod_stream_management, set_ack_freq, [1]),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus_ejabberd:rpc(mod_stream_management, set_ack_freq, [1]),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

simple_ack(Config) ->
    escalus:story(Config, [{alice_sm, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice sends a message to Bob
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

        %% Bob gets the message
        escalus:assert(is_chat_message, [<<"OH, HAI!">>], escalus:wait_for_stanza(Bob))

    end).

ack_after_four_messages(Config) ->
    escalus_ejabberd:rpc(mod_stream_management, set_ack_freq, [4]),
    escalus:story(Config, [{alice_sm, 1}, {bob, 1}], fun(Alice, Bob) ->

    Msgs = [<<"Brown Eyes and I were tired">>,
            <<"We had walked and we had scrambled">>,
            <<"Through the moors and through the briars">>,
            <<"Through the endless blue meanders">>,
            <<"In the blue August moon">>,
            <<"In the cool August moon">>,
            <<"Over the nights and through the fires">>,
            <<"We went surging down the wires">>,
            <<"Through the towns and on the highways">>,
            <<"Through the storms in all their thundering">>,
            <<"In the blue August moon">>,
            <<"In the cool August moon">>
           ],
        [escalus:send(Bob, escalus_stanza:chat_to(Alice, M)) || M <- Msgs],

    [escalus:assert(is_chat_message, [M], escalus:wait_for_stanza(Alice))
     || M <- Msgs]
    end).


no_ack(Config) ->
    escalus_users:update_userspec(Config, alice, stream_management, {true, manual_ack}),
    Msg = <<"Now is the winter of our discontent">>,
    escalus_ejabberd:rpc(mod_stream_management, set_ack_freq, [3]),
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        escalus:send(Bob, escalus_stanza:chat_to(Alice, Msg)),
        escalus:assert(is_chat_message, [Msg], escalus:wait_for_stanza(Alice)),
        escalus:assert(is_ack_request, escalus:wait_for_stanza(Alice))
    end).
