-module(stream_automation_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, automatic},
     {group, manual}].

groups() ->
    [{automatic, [shuffle, {repeat, 3}], [simple_ack, ack_after_four_messages]},
     {manual, [shuffle, {repeat, 3}], [ack_request_is_visible]}].

suite() ->
    [{require, ejabberd_domain}] ++ escalus:suite().

%% Turn mod_offline off for the duration of the tests.
no_mod_offline() ->
    {no_mod_offline,
     fun no_mod_offline_setup/0,
     fun no_mod_offline_reset/1,
     not_running}.

no_mod_offline_setup() ->
    Domain = get_domain(),
    case escalus_ejabberd:rpc(gen_mod, is_loaded,
                              [Domain, mod_offline]) of
        true ->
            escalus_ejabberd:rpc(gen_mod, stop_module,
                                 [Domain, mod_offline]),
            running;
        false ->
            not_running
    end.

no_mod_offline_reset(not_running) -> ok;
no_mod_offline_reset(running) ->
    Domain = get_domain(),
    escalus_ejabberd:rpc(gen_mod, start_module, [Domain, mod_offline, []]).

get_domain() ->
    case ct:get_config(ejabberd_domain, undefined) of
        undefined -> error({not_found, ejabberd_domain});
        Domain -> Domain
    end.

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    NewConfig = escalus_ejabberd:setup_option(no_mod_offline(), Config),
    escalus:init_per_suite(NewConfig).

end_per_suite(Config) ->
    NewConfig = escalus_ejabberd:reset_option(no_mod_offline(), Config),
    escalus:end_per_suite(NewConfig).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus_ejabberd:rpc(mnesia, clear_table, [offline_msg]),
    escalus_ejabberd:rpc(mod_stream_management, set_ack_freq, [1]),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus_ejabberd:rpc(mod_stream_management, set_ack_freq, [1]),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

%% @doc The server is (by default) set to send ack requests after every message.
%% Alice would not get the message from Bob unless all the ack requests
%% before it were handled.

simple_ack(Config) ->
    escalus:story(Config, [{alice_sm, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice sends a message to Bob
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

        %% Bob gets the message
        escalus:assert(is_chat_message, [<<"OH, HAI!">>], escalus:wait_for_stanza(Bob)),

        %% Bob sends to Alice
        escalus:send(Bob, escalus_stanza:chat_to(Alice,
                                                 <<"Let's have a deep conversation!">>)),

        %% Alice gets the message. No acks were necessary
        escalus:assert(is_chat_message, [<<"Let's have a deep conversation!">>],
                       escalus:wait_for_stanza(Alice))


    end).

%% This test shows that <r> elements never interfere with regular messages,
%% because the client handles them transparently. We don't need to account for
%% them in escalus stories.
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
            <<"In the blue August moon!">>,
            <<"In the cool August moon!">>
           ],
        [escalus:send(Bob, escalus_stanza:chat_to(Alice, M)) || M <- Msgs],

    [escalus:assert(is_chat_message, [M], escalus:wait_for_stanza(Alice))
     || M <- Msgs]
    end).


%% @doc Show that if we set manual_ack to true, then the client will actually see
%% server-side ack requests and we can access them in the test.
ack_request_is_visible(Config) ->
    Config2 = escalus_users:update_userspec(Config, alice_sm, manual_ack, true),
    Msg = <<"Now is the winter of our discontent">>,

    escalus_ejabberd:rpc(mod_stream_management, set_ack_freq, [2]),
    escalus:story(Config2, [{alice_sm, 1}, {bob, 1}], fun(Alice, Bob) ->
        escalus:send(Bob, escalus_stanza:chat_to(Alice, Msg)),
        escalus:assert(is_chat_message, [Msg], escalus:wait_for_stanza(Alice)),
        escalus:assert(is_sm_ack_request, escalus:wait_for_stanza(Alice))
    end).
