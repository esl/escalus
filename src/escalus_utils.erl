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
         distinct_pairs/2,
         distinct_ordered_pairs/2,
         each_with_index/3,
         all_true/1,
         any_true/1,
         identity/1,
         mix_match/3,
         get_jid/1,
         get_short_jid/1,
         jid_to_lower/1,
         get_username/1,
         get_server/1,
         get_resource/1,
         drop_first_such/2,
         show_backtrace/0,
         is_prefix/2,
         start_clients/1,
         start_clients/2,
         regexp_get/2]).

-import(escalus_compat, [unimplemented/0, bin/1]).

-include("escalus.hrl").
-include_lib("exml/include/exml.hrl").

-type jid_spec() :: #client{} | atom() | binary() | string().
-export_type([jid_spec/0]).

-spec log_stanzas(iolist(), [exml:element()]) -> any().
log_stanzas(Comment, Stanzas) ->
    error_logger:info_msg("~s:~s~n", [Comment, stanza_lines("\n  * ", Stanzas)]).

-spec pretty_stanza_list([exml:element()]) -> string().
pretty_stanza_list(Stanzas) ->
    binary_to_list(list_to_binary(stanza_lines("     ", Stanzas))).

%% calls Fun(A, B) on each distinct (A =/= B) pair of elements in List
%% if Fun(A, B) was called, then Fun(B, A) won't
-spec distinct_pairs(fun((T, T) -> any()), [T]) -> integer().
distinct_pairs(Fun, List) ->
    K = each_with_index(fun(A, N) ->
        each_with_index(fun(B, M) ->
            if N < M ->
                    Fun(A, B);
               true ->
                   skip
           end
        end, 0, List)
    end, 0, List),
    K * (K - 1) div 2.

%% calls Fun(A, B) on each distinct (A =/= B) ordered pair of elements in List
-spec distinct_ordered_pairs(fun((T, T) -> any()), [T]) -> integer().
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
-spec each_with_index(fun((T, integer()) -> any()), _, [T]) -> integer().
each_with_index(Fun, Start, List) ->
    lists:foldl(fun(Element, N) ->
        Fun(Element, N),
        N + 1
    end, Start, List).

-spec all_true([boolean()]) -> boolean().
all_true(List) ->
    lists:foldl(fun erlang:'and'/2, true, List).

-spec any_true([boolean()]) -> boolean().
any_true(List) ->
    lists:foldl(fun erlang:'or'/2, false, List).

-spec identity(A) -> A.
identity(X) ->
    X.

%% Does for each Case in Cases exist a Cond in Conds such that
%% (Predgen(Cond))(Case) == true?
-spec mix_match(fun((A) -> fun((B) -> boolean())), [A], [B]) -> boolean().
mix_match(Predgen, Conds, Cases) ->
    [] == lists:foldl(fun(Cond, CasesLeft) ->
              Pred = Predgen(Cond),
              drop_first_such(Pred, CasesLeft)
          end, Cases, Conds).

-spec drop_first_such(fun((A) -> boolean()), [A]) -> [A].
drop_first_such(Pred, List) ->
    drop_first_such(Pred, List, []).

drop_first_such(_, [], Acc) ->
    lists:reverse(Acc);
drop_first_such(Pred, [H|T], Acc) ->
    case Pred(H) of
        true ->
            lists:reverse(Acc, T);
        false ->
            drop_first_such(Pred, T, [H|Acc])
    end.

stanza_lines(Prefix, Stanzas) ->
    [[Prefix, exml:to_iolist(S)] || S <- Stanzas].

-spec show_backtrace() -> any().
show_backtrace() ->
    try throw(catch_me)
    catch _:_:S ->
        error_logger:info_msg("Backtrace:~n~p~n", [tl(S)])
    end.

-spec get_jid(jid_spec()) -> binary().
get_jid(#client{jid=Jid}) ->
    Jid;
get_jid(Username) when is_atom(Username) ->
    %% TODO: Drop this clause since it relies on external state - the config file.
    %%       Define users in a test suite and pass them around as needed.
    Complaint = io_lib:format("~s:get_jid/1: an atom() passed as Username - this clause will "
                              "be dropped in the future as it relies on an external config file",
                              [?MODULE]),
    escalus_compat:complain(Complaint),
    bin(escalus_users:get_jid([], Username));
get_jid(Jid) when is_list(Jid) ->
    bin(Jid);
get_jid(Jid) when is_binary(Jid) ->
    Jid.

-spec get_short_jid(#client{} | atom() | binary() | string()) -> binary().
get_short_jid(#client{}=Recipient) ->
    escalus_client:short_jid(Recipient);
get_short_jid(Username) when is_atom(Username) ->
    escalus_users:get_jid([], Username);
get_short_jid(Jid) when is_list(Jid) ->
    get_short_jid(list_to_binary(Jid));
get_short_jid(Jid) when is_binary(Jid) ->
    regexp_get(Jid, <<"^([^@]*[@][^/]*)">>).

-spec jid_to_lower(binary()) -> binary().
jid_to_lower(Jid) ->
    %% simplified lowercaseing
    list_to_binary(string:to_lower(binary_to_list(Jid))).

-spec get_username(UserOrClient :: jid_spec()) -> binary().
get_username(UserOrClient) ->
    regexp_get(get_short_jid(UserOrClient), <<"^([^@]*)">>).

-spec get_server(UserOrClient :: jid_spec()) -> binary().
get_server(UserOrClient) ->
    regexp_get(get_short_jid(UserOrClient), <<"^[^@]*[@]([^/]*)">>).

-spec get_resource(JID :: binary()) -> binary().
get_resource(JID) ->
    regexp_get(JID, <<"^[^/]+/(.*)$">>).

-spec is_prefix(binary(), binary()) -> boolean().
is_prefix(Prefix, Full) when is_binary(Prefix), is_binary(Full) ->
    LCP = binary:longest_common_prefix([Prefix, Full]),
    size(Prefix) =< size(Full) andalso LCP == size(Prefix).

-spec start_clients([{_, _}]) -> {pid(), list(pid())}.
start_clients(Clients) ->
    start_clients([], Clients).

-spec start_clients(escalus_config:config(), [{_, _}]) -> {pid(), list(pid())}.
start_clients(Config0, ClientRecipes) ->
    AllCDs = escalus_config:get_config(escalus_users, Config0),
    FlatCDs = [{CD, Res} || {Username, Resources} <- ClientRecipes,
                            {_, CD} <- [lists:keyfind(Username, 1, AllCDs)],
                            Res <- Resources],
    Config1 = [{_, Cleaner} | _] = escalus_cleaner:start(Config0),
    Clients = escalus_overridables:do(Config0,
                                      start_ready_clients, [Config1, FlatCDs],
                                      {escalus_story, start_ready_clients}),
    {Cleaner, Clients}.

-spec regexp_get(binary(), binary()) -> binary().
regexp_get(Jid, Regex) ->
    {match, [ShortJid]} =
        re:run(Jid, Regex, [{capture, all_but_first, binary}]),
    ShortJid.
