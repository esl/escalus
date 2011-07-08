%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(escalus_utils).

-export([log_stanzas/2, exchange_stanzas/2]).

-include("include/escalus.hrl").
-include_lib("exmpp/include/exmpp.hrl").

-spec log_stanzas(iolist(), [#xmlel{}]) -> any().
log_stanzas(Comment, Stanzas) ->
    StanzaLines = [["\n  * ", exmpp_xml:document_to_iolist(S)] || S <- Stanzas],
    error_logger:info_msg("~s:~s~n", [Comment, StanzaLines]).

-spec exchange_stanzas([#client{}], #xmlel{}) -> any().
exchange_stanzas(Clients, BaseStanza) ->
    % send BaseStanza to everyone
    all_to_all(fun(Sender, #client{jid=RecipientJid}) ->
        Stanza = exmpp_stanza:set_recipient(BaseStanza, RecipientJid),
        escalus_client:send(Sender, Stanza)
    end, Clients, Clients),

    % wait for everyone to receive it
    StanzaCount = length(Clients) - 1, % get BaseStanza from everyone else, but me (-1)
    lists:foreach(fun(Client) ->
        escalus_client:wait_for_stanzas(Client, StanzaCount)
    end, Clients).

%% As and Bs are lists of equal length
%% calls Fun(A, B) on all but corresponding elements
%% of that lists
all_to_all(Fun, As, Bs) ->
    lists:foldl(fun(A, N) ->
        lists:foldl(fun(B, M) ->
            if N =/= M ->
                    Fun(A, B);
               true ->
                   skip
           end,
           M + 1
        end, 1, Bs),
        N + 1
    end, 1, As).
