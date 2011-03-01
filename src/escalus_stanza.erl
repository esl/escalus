-module(escalus_stanza).

-export([chat_to/2]).

-include("include/escalus.hrl").
-include_lib("exmpp/include/exmpp.hrl").

chat_to(#client{jid=Jid}, Msg) ->
    Chat = exmpp_message:chat(Msg),
    exmpp_stanza:set_recipient(Chat, Jid).
