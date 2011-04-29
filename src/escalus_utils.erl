-module(escalus_utils).

-export([log_stanzas/2]).

log_stanzas(Comment, Stanzas) ->
    StanzaLines = [["\n  * ", exmpp_xml:document_to_iolist(S)] || S <- Stanzas],
    error_logger:info_msg("~s:~s~n", [Comment, StanzaLines]).
