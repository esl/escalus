%%%-------------------------------------------------------------------
%%% @author Denys Gonchar
%%% @copyright (C) 2018, Erlang Solutions Ltd.
%%% @doc
%%%   this suite is testing escalus_component behaviour.
%%%   ensure that MongooseIM is started before run this suite.
%%% @end
%%% Created : 26. Apr 2018 15:45
%%%-------------------------------------------------------------------
-module(component_SUITE).
-author("denys").
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-define(COMPONENT_MOD, test_component).
-define(COMPONENT_JID, <<"echo.localhost">>).


all() ->
    [component_pid_registration,
     second_registration_attempt,
     test_msg, test_handle_info].

init_per_suite(Config) ->
    meck:new(?COMPONENT_MOD, [non_strict, no_link]),
    meck:expect(?COMPONENT_MOD, init, 1, {ok, 1}),
    meck:expect(?COMPONENT_MOD, process_stanza, fun process_stanza/3),
    meck:expect(?COMPONENT_MOD, terminate, fun terminate/2),
    NewConfig = escalus:init_per_suite(Config),
    ComponentConnectionArgs = [{host, ct:get_config(ejabberd_addr)},
                               {password, ct:get_config(ejabberd_service_password)},
                               {port, ct:get_config(ejabberd_service_port)},
                               {component, ?COMPONENT_JID}],
    ct:print("connection args ~p", [ComponentConnectionArgs]),
    {ok, Pid} = escalus_component:start({local, ?COMPONENT_MOD}, ?COMPONENT_MOD,
                                        ComponentConnectionArgs, []),
    [{component_pid, Pid},
     {component_connection_args, ComponentConnectionArgs},
     {component_module, ?COMPONENT_MOD} |
     NewConfig].

end_per_suite(Config) ->
    Module = proplists:get_value(component_module, Config),
    Pid = proplists:get_value(component_pid, Config),
    escalus_component:stop(Pid,self()),
    receive
        component_stopped ->
            meck:unload(Module),
            escalus:end_per_suite(Config),
            Config
        after 10000 ->
            throw("escalus_component:stop/2 failed")
    end.

test_msg(_Config) ->
    [{alice, Cfg}] = Users = escalus:get_users([alice]),
    escalus_users:create_users([], Users),
    {ok, Alice, _Features} = escalus_connection:start(Cfg),
    Msg = escalus_stanza:chat_to(?COMPONENT_JID, <<"Hi!">>),
    escalus:send(Alice, Msg),
    escalus:assert(is_chat_message_from_to,
                   [?COMPONENT_JID,
                    escalus_client:full_jid(Alice),
                    <<"Hi!">>],
                   escalus:wait_for_stanza(Alice)).

test_handle_info(Config) ->
    Pid = proplists:get_value(component_pid, Config),
    Pid!self(),%trying to send something before mocking handle_info/3
    meck:expect(?COMPONENT_MOD, handle_info, fun handle_info/3),
    Pid!self(),
    Pid!self(),
    N = receive
            {state, N1} -> N1
            after 500 -> throw("handle_info/3 callback failed")
        end,
    receive
        {state, N2} when N2 =:= N + 1 -> ok
        after 500 -> throw("handle_info/3 callback failed")
    end.

component_pid_registration(Config) ->
    Pid = proplists:get_value(component_pid, Config),
    Name = proplists:get_value(component_module, Config),
    ?assertEqual(Pid, whereis(Name)).

second_registration_attempt(Config) ->
    ComponentMod = proplists:get_value(component_module, Config),
    ComponentConnectionArgs = proplists:get_value(component_connection_args, Config),
    ?assertMatch({error,_},escalus_component:start(ComponentMod, ComponentConnectionArgs, [])).
%%%-------------------------------------------------------------------
%%% meck functions
%%%-------------------------------------------------------------------
process_stanza(Stanza, Client, N) ->
    [From, To] = [exml_query:attr(Stanza, X) || X <- [<<"from">>, <<"to">>]],
    Text = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
    EchoMsg = escalus_stanza:chat(To, From, Text),
    escalus:send(Client, EchoMsg),
    {ok, N + 1}.

terminate(Pid,_N) ->
    io:format("~nterm/2 ~p~n",[Pid]),
    Pid!component_stopped.

handle_info(Pid, _Client, N) ->
    Pid!{state,N},
    {ok,N+1}.