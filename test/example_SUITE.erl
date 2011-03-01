-module(example_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, messages}].

groups() ->
    [{messages, [sequence], [messages_story]}].

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
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

messages_story(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        % Alice sends a message to Bob
        escalus_client:send_wait(Alice, escalus_stanza:chat_to(Bob, "OH, HAI!")),

        % Bob gets the message
        escalus_assert:is_chat_message("OH, HAI!", escalus_client:only_stanza(Bob))

    end).
