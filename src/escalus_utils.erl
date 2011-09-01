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
         pretty_stanza_list/1,
         exchange_stanzas/3,
         distinct_ordered_pairs/2,
         each_with_index/3,
         all_true/1,
         any_true/1,
         mix_match/3,
         drop_first_such/2
        ]).

-include("include/escalus.hrl").
-include_lib("exmpp/include/exmpp.hrl").

-spec log_stanzas(iolist(), [#xmlel{}]) -> any().
log_stanzas(Comment, Stanzas) ->
    error_logger:info_msg("~s:~s~n", [Comment, stanza_lines("\n  * ", Stanzas)]).

-spec pretty_stanza_list([#xmlel{}]) -> string().
pretty_stanza_list(Stanzas) ->
    binary_to_list(list_to_binary(stanza_lines("     ", Stanzas))).

-spec exchange_stanzas([#client{}], #xmlel{}, full_jid|short_jid) -> any().
exchange_stanzas(Clients, BaseStanza, JidType) ->
    % send BaseStanza to everyone
    distinct_ordered_pairs(fun(Sender, Recipient) ->
        RecipientJid = escalus_client:JidType(Recipient),
        Stanza = exmpp_stanza:set_recipient(BaseStanza, RecipientJid),
        escalus_client:send(Sender, Stanza)
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

all_true(List) ->
    lists:foldl(fun erlang:'and'/2, true, List).

any_true(List) ->
    lists:foldl(fun erlang:'or'/2, false, List).

%% Does for each Case in Cases exist a Cond in Conds such that
%% (Predgen(Cond))(Case) == true?
mix_match(Predgen, Conds, Cases) ->
    [] == lists:foldl(fun(Cond, CasesLeft) ->
              Pred = Predgen(Cond),
              drop_first_such(Pred, CasesLeft)
          end, Cases, Conds).

drop_first_such(Pred, List) ->
    drop_first_such(Pred, List, []).

drop_first_such(Pred, [], Acc) ->
    lists:reverse(Acc);
drop_first_such(Pred, [H|T], Acc) ->
    case Pred(H) of
        true ->
            lists:reverse(Acc) ++ T;
        false ->
            drop_first_such(Pred, T, [H|Acc])
    end.

stanza_lines(Prefix, Stanzas) ->
    [[Prefix, exmpp_xml:document_to_iolist(S)] || S <- Stanzas].
