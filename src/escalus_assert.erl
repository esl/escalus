-module(escalus_assert).

-export([is_chat_message/2]).

is_chat_message(Msg, Stanza) when is_list(Msg) ->
    is_chat_message(list_to_binary(Msg), Stanza);
is_chat_message(Msg, Stanza) when is_binary(Msg) ->
    chat = exmpp_message:get_type(Stanza),
    Msg = exmpp_message:get_body(Stanza).
