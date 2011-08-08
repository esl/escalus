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

-export([log_stanzas/2,
         exchange_stanzas/3,
         distinct_ordered_pairs/2,
         each_with_index/3
        ]).

-include("include/escalus.hrl").
-include_lib("exmpp/include/exmpp.hrl").

-spec log_stanzas(iolist(), [#xmlel{}]) -> any().
log_stanzas(Comment, Stanzas) ->
    StanzaLines = [["\n  * ", exmpp_xml:document_to_iolist(S)] || S <- Stanzas],
    error_logger:info_msg("~s:~s~n", [Comment, StanzaLines]).

-spec exchange_stanzas([#client{}], #xmlel{}, full_jid|short_jid) -> any().
exchange_stanzas(Clients, BaseStanza, JidType) ->
    % send BaseStanza to everyone
    distinct_ordered_pairs(fun(Sender, #client{jid=RecipientJid}) ->
        Jid = case JidType of
            full_jid -> RecipientJid;
            short_jid -> make_short_jid(RecipientJid)
        end,
        Stanza = exmpp_stanza:set_recipient(BaseStanza, Jid),
        escalus_client:send(Sender, Stanza)
    end, Clients),

    % wait for everyone to receive it
    StanzaCount = length(Clients) - 1, % get BaseStanza from everyone else, but me (-1)
    lists:foreach(fun(Client) ->
        escalus_client:wait_for_stanzas(Client, StanzaCount)
    end, Clients).

%% calls Fun(A, B) on each distinct (A =/= B) ordered pair of elements in List
distinct_ordered_pairs(Fun, List) ->
    K = each_with_index(fun(A, N) ->
        each_with_index(fun(B, M) ->
            if N =/= M ->
                    Fun(A, B);
               true ->
                   skip
           end
        end, 0, List)
    end, 0, List),
    K * (K - 1).

%% Calls Fun(Element, Index) for indices (starting from Start) and elements of List
each_with_index(Fun, Start, List) ->
    lists:foldl(fun(Element, N) ->
        Fun(Element, N),
        N + 1
    end, Start, List).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
make_short_jid(Jid) ->
    {match, [ShortJid]} =
        re:run(Jid, <<"^([^/]*)">>, [{capture, all_but_first, binary}]),
    ShortJid.
