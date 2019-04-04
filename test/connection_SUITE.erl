%% @doc Tests for escalus_connection

-module(connection_SUITE).

-include_lib("exml/include/exml_stream.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0,
         groups/0]).

-export([wait/1,
         wait_with_handler/1,
         receive_stanza_timeout/1,
         receive_stanza_timeout_safe/1,
         receive_stanza_with_metadata/1,
         receive_stanza_pred/1,
         receive_stanza_pred_with_handler/1,
         receive_stanza_assert/1
        ]).

%% Common Test callbacks

all() ->
    [{group, basic}].

groups() ->
    [{basic, [parallel], [wait,
                          wait_with_handler,
                          receive_stanza_timeout,
                          receive_stanza_timeout_safe,
                          receive_stanza_with_metadata,
                          receive_stanza_pred,
                          receive_stanza_pred_with_handler,
                          receive_stanza_assert]}].

%% Test cases

wait(_Config) ->
    %% Wait for 10 ms, no handlers, no incoming stanzas
    ok = escalus_connection:wait(client(), 10).

wait_with_handler(_Config) ->
    Handler = handler(fun(Msg) -> escalus_pred:is_chat_message(msg(), Msg) end,
                      fun(Msg) -> self() ! {handled, Msg} end),
    Client = set_received_handlers(client(), [Handler]),
    HandledStanza = escalus_stanza:chat_to(my_jid(), msg()),
    OtherStanza = escalus_stanza:chat_to(my_jid(), msg2()),
    self() ! escalus_connection:stanza_msg(OtherStanza, metadata()),
    self() ! escalus_connection:stanza_msg(HandledStanza, metadata()),
    ok = escalus_connection:wait(Client, 10),
    receive {handled, HandledStanza} -> ok after 0 -> ct:fail("not handled: ~p", [HandledStanza]) end.

receive_stanza_timeout(_Config) ->
    Client = client(),
    Stanza = escalus_stanza:chat_to(my_jid(), msg()),
    erlang:send_after(1000, self(), escalus_connection:stanza_msg(Stanza, metadata())),

    %% Waits for 10 ms, but the stanza is sent later
    ?assertException(throw, timeout, escalus_connection:receive_stanza(Client, #{timeout => 10})),

    %% The same but with a named timeout message, the two calls below are identical
    ?assertException(throw, {timeout, msg}, escalus_connection:get_stanza(Client, msg, 10)),
    ?assertException(throw, {timeout, msg},
                     escalus_connection:receive_stanza(Client, #{timeout => 10, name => msg})),

    Stanza = escalus_connection:receive_stanza(Client, #{}).

receive_stanza_timeout_safe(_Config) ->
    Client = client(),
    Stanza = escalus_stanza:chat_to(my_jid(), msg()),
    erlang:send_after(1000, self(), escalus_connection:stanza_msg(Stanza, metadata())),

    %% Waits for 10 ms, but the stanza is sent later, the two calls below are identical
    {error, timeout} = escalus_connection:receive_stanza(Client, #{timeout => 10, safe => true}),
    {error, timeout} = escalus_connection:get_stanza_safe(Client, 10),

    Stanza = escalus_connection:receive_stanza(Client, #{}).

receive_stanza_with_metadata(_Config) ->
    Client = client(),
    Stanza = escalus_stanza:chat_to(my_jid(), msg()),

    Metadata1 = metadata(),
    self() ! escalus_connection:stanza_msg(Stanza, Metadata1),
    {Stanza, Metadata1} = escalus_connection:receive_stanza(Client, #{with_metadata => true}),

    %% The same test but with a helper function
    Metadata2 = metadata(),
    self() ! escalus_connection:stanza_msg(Stanza, Metadata2),
    {Stanza, Metadata2} = escalus_connection:get_stanza_with_metadata(Client, msg, 1000).

receive_stanza_pred(_Config) ->
    Client = client(),
    SkippedStanza = escalus_stanza:chat_to(my_jid(), msg()),
    Stanza = escalus_stanza:chat_to(my_jid(), msg2()),
    Pred = fun(S) -> escalus_pred:is_chat_message(msg2(), S) end,

    self() ! escalus_connection:stanza_msg(SkippedStanza, metadata()),
    self() ! escalus_connection:stanza_msg(Stanza, metadata()),

    %% The predicate should filter out the first stanza, so we receive only the second one
    Stanza = escalus_connection:receive_stanza(Client, #{pred => Pred}),
    ?assertException(throw, timeout, escalus_connection:receive_stanza(Client, #{timeout => 10})).

receive_stanza_pred_with_handler(_Config) ->
    Handler = handler(fun(_) -> true end, fun(Msg) -> self() ! {skipped, Msg} end),
    Client = set_received_handlers(client(), [Handler]),
    SkippedStanza = escalus_stanza:chat_to(my_jid(), msg()),
    Stanza = escalus_stanza:chat_to(my_jid(), msg2()),
    Pred = fun(S) -> escalus_pred:is_chat_message(msg2(), S) end,

    %% Filtered out by Pred, but handled by Handler
    self() ! escalus_connection:stanza_msg(SkippedStanza, metadata()),
    ?assertException(throw, timeout, escalus_connection:receive_stanza(Client, #{timeout => 10, pred => Pred})),
    receive {skipped, SkippedStanza} -> ok after 0 -> ct:fail("not skipped: ~p", [SkippedStanza]) end,

    %% Accepted by Pred, so not handled
    self() ! escalus_connection:stanza_msg(Stanza, metadata()),
    Stanza = escalus_connection:receive_stanza(Client, #{timeout => 10, pred => Pred}),
    receive {skipped, Stanza} -> ct:fail("skipped: ~p", [SkippedStanza]) after 0 -> ok end.

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
