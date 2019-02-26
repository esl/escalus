%% @doc Tests for escalus_connection

-module(connection_SUITE).

-include_lib("exml/include/exml_stream.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0,
         groups/0]).

-export([receive_stanza_timeout/1,
         receive_stanza_timeout_safe/1,
         receive_stanza_with_metadata/1,
         receive_stanza_pred/1,
         receive_stanza_assert/1,
         receive_stanza_with_handler/1]).

%% Common Test callbacks

all() ->
    [{group, basic}].

groups() ->
    [{basic, [parallel], [receive_stanza_timeout,
                          receive_stanza_timeout_safe,
                          receive_stanza_with_metadata,
                          receive_stanza_pred,
                          receive_stanza_assert,
                          receive_stanza_with_handler]}].

%% Test cases

receive_stanza_timeout(_Config) ->
    Client = client(),
    Stanza = escalus_stanza:chat_to(my_jid(), msg()),
    erlang:send_after(1000, self(), escalus_connection:stanza_msg(Stanza, metadata())),
    ?assertException(throw, timeout, escalus_connection:receive_stanza(Client, #{timeout => 10})),
    ?assertException(throw, {timeout, msg},
                     escalus_connection:receive_stanza(Client, #{timeout => 10, name => msg})),
    Stanza = escalus_connection:receive_stanza(Client, #{}).

receive_stanza_timeout_safe(_Config) ->
    Client = client(),
    Stanza = escalus_stanza:chat_to(my_jid(), msg()),
    erlang:send_after(1000, self(), escalus_connection:stanza_msg(Stanza, metadata())),
    {error, timeout} = escalus_connection:receive_stanza(Client, #{timeout => 10, safe => true}),
    Stanza = escalus_connection:receive_stanza(Client, #{}).

receive_stanza_with_metadata(_Config) ->
    Client = client(),
    Stanza = escalus_stanza:chat_to(my_jid(), msg()),
    Metadata = metadata(),
    self() ! escalus_connection:stanza_msg(Stanza, Metadata),
    {Stanza, Metadata} = escalus_connection:receive_stanza(Client, #{with_metadata => true}).

receive_stanza_pred(_Config) ->
    Client = client(),
    SkippedStanza = escalus_stanza:chat_to(my_jid(), msg()),
    Stanza = escalus_stanza:chat_to(my_jid(), msg2()),
    self() ! escalus_connection:stanza_msg(SkippedStanza, metadata()),
    self() ! escalus_connection:stanza_msg(Stanza, metadata()),
    Pred = fun(S) -> escalus_pred:is_chat_message(msg2(), S) end,
    Stanza = escalus_connection:receive_stanza(Client, #{pred => Pred}),
    ?assertException(throw, timeout, escalus_connection:receive_stanza(Client, #{timeout => 10})).

receive_stanza_assert(_Config) ->
    Client = client(),
    Stanza = escalus_stanza:chat_to(my_jid(), msg2()),
    self() ! escalus_connection:stanza_msg(Stanza, metadata()),
    ?assertException(error, {assertion_failed, assert, is_chat_message, _, _, _},
                     escalus_connection:receive_stanza(Client, #{assert => {is_chat_message, [msg()]}})),
    self() ! escalus_connection:stanza_msg(Stanza, metadata()),
    Stanza = escalus_connection:receive_stanza(Client, #{assert => {is_chat_message, [msg2()]}}),
    self() ! escalus_connection:stanza_msg(Stanza, metadata()),
    Stanza = escalus_connection:receive_stanza(Client, #{assert => fun(S) -> S =:= Stanza end}),
    self() ! escalus_connection:stanza_msg(Stanza, metadata()),
    Stanza = escalus_connection:receive_stanza(Client, #{assert => is_chat_message}).

receive_stanza_with_handler(_Config) ->
    Handler = handler(fun(Msg) -> escalus_pred:is_chat_message(msg(), Msg) end,
                      fun(Msg) -> self() ! {handled, Msg} end),
    Client = set_received_handlers(client(), [Handler]),
    HandledStanza = escalus_stanza:chat_to(my_jid(), msg()),
    OtherStanza = escalus_stanza:chat_to(my_jid(), msg2()),
    self() ! escalus_connection:stanza_msg(HandledStanza, metadata()),
    self() ! escalus_connection:stanza_msg(OtherStanza, metadata()),
    ?assertException(throw, timeout,
                     escalus_connection:receive_stanza(Client, #{pred => fun(_) -> false end, timeout => 10})),
    receive {handled, HandledStanza} -> ok after 0 -> ct:fail("not handled: ~p", [HandledStanza]) end.

%% Helpers

handler(Pred, Action) ->
    fun(_Client, Msg) ->
            case Pred(Msg) of
                true -> Action(Msg), true;
                false  -> false
            end
    end.

my_jid() -> <<"alice@localhost">>.

msg() -> <<"Message">>.

msg2() -> <<"Message 2">>.

metadata() -> #{recv_timestamp => os:system_time(micro_seconds)}.

client() -> #client{jid = my_jid(), rcv_pid = self(), props = []}.

set_received_handlers(Client, Handlers) ->
    Client#client{props = [{received_stanza_handlers, Handlers}]}.
