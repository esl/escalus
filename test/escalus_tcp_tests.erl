%%%===================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd.
%%%===================================================================

-module(escalus_tcp_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("src/escalus_tcp.hrl").

interleave_msgs_and_rs_test() ->
    meck:new(gen_tcp, [unstick]),
    meck:expect(gen_tcp, send, fun(_, _) -> ok end),
    meck:new(escalus_event),
    meck:expect(escalus_event, incoming_stanza, fun(_, _) -> ok end),
    Msg = escalus_stanza:chat_to(<<"Bob@localhost">>, <<"Hi!">>),
    R = escalus_stanza:sm_request(),
    FirstStanzas = [Msg, R, Msg],
    SecondStanzas = [R, Msg, R],
    State = #state{owner = self(),
                   sm_state = {true, 0, active},
                   event_client = self()},
    SecondState = escalus_tcp:forward_to_owner(FirstStanzas, State, os:system_time(microsecond)),
    #state{sm_state = SMState} =
        escalus_tcp:forward_to_owner(SecondStanzas, SecondState, os:system_time(microsecond)),
    ?assertEqual({true, 3, active}, SMState),
    meck:unload(escalus_event),
    meck:unload(gen_tcp).

