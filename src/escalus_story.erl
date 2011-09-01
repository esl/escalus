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

-module(escalus_story).

% Public API
-export([story/3,
         make_everyone_friends/2,
         start_ready_clients/2]).

-include("escalus.hrl").
-include_lib("test_server/include/test_server.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

story(Config, ResourceCounts, Story) ->
    {escalus_users, NamedSpecs} = proplists:lookup(escalus_users, Config),
    ClientDescs = [[{UserSpec, "res"++integer_to_list(N)} || N <- lists:seq(1, ResCount)] ||
                   {{_, UserSpec}, ResCount} <- zip_shortest(NamedSpecs,
                                                             ResourceCounts)],
    try
        Clients = start_clients(Config, ClientDescs),
        ensure_all_clean(Clients),
        apply(Story, Clients),
        post_story_checks(Config, Clients)
    after
        escalus_cleaner:clean(Config)
    end.

make_everyone_friends(Config0, Users) ->
    % start the clients
    Config1 = escalus_cleaner:start(Config0),
    Clients = start_clients(Config1, [[{US, "friendly"}] || {_Name, US} <- Users]),

    % exchange subscribe and subscribed stanzas
    escalus_utils:distinct_ordered_pairs(fun(C1, C2) ->
        send_presence(C1, subscribe, C2),
        wait_for_presence("subscribe", C2),
        send_presence(C2, subscribed, C1),
        wait_for_presence("subscribed", C1)
    end, Clients),

    % stop the clients
    escalus_cleaner:clean(Config1),
    escalus_cleaner:stop(Config1),
    [{everyone_is_friends, true} | Config1].

start_ready_clients(Config, FlatCDs) ->
    {_, RClients} = lists:foldl(fun({UserSpec, Resource}, {N, Acc}) ->
        Client = escalus_client:start(Config, UserSpec, Resource),
        escalus_hooks:run(Config, after_login, [Client]),
        escalus_client:send(Client, escalus_stanza:presence(available)),
        %% drop 1 own presence + N-1 probe replies = N presence stanzas
        drop_presences(Client, N),
        {N+1, [Client|Acc]}
    end, {1, []}, FlatCDs),
    Clients = lists:reverse(RClients),
    ClientsCount = length(Clients),
    escalus_utils:each_with_index(fun(Client, N) ->
        %% drop presence updates of guys who have logged in after you did
        drop_presences(Client, ClientsCount - N)
    end, 1, Clients),
    ensure_all_clean(Clients),
    Clients.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

send_presence(From, Type, To) ->
    ToJid = escalus_client:short_jid(To),
    Stanza = exmpp_stanza:set_recipient(exmpp_presence:Type(), ToJid),
    escalus_client:send(From, Stanza).

wait_for_presence(Type, Client) ->
    Stanza = escalus_client:wait_for_stanza(Client),
    case escalus_pred:is_presence_type(Type, Stanza) of
        true ->
            Stanza;
        false ->
            wait_for_presence(Type, Client)
    end.

ensure_all_clean(Clients) ->
    %% timer:sleep(1000),
    lists:foreach(fun(Client) ->
        escalus_assert:has_no_stanzas(Client)
    end, Clients).

start_clients(Config, ClientDescs) ->
    case proplists:get_bool(everyone_is_friends, Config) of
        true ->
            start_ready_clients(Config, lists:flatten(ClientDescs));
        false ->
            lists:flatmap(fun(UserCDs) ->
                start_ready_clients(Config, UserCDs)
            end, ClientDescs)
    end.

drop_presences(Client, N) ->
    Dropped = escalus_client:wait_for_stanzas(Client, N),
    lists:foreach(fun escalus_assert:is_presence_stanza/1, Dropped),
    N = length(Dropped).

post_story_checks(Config, Clients) ->
    case proplists:get_bool(escalus_no_stanzas_after_story, Config) of
        Value ->
            lists:foreach(
                fun escalus_assert:has_no_stanzas/1,
                Clients
            );
        _ ->
            do_nothing
    end.

zip_shortest([H1|T1], [H2|T2]) ->
    [{H1,H2}|zip_shortest(T1, T2)];
zip_shortest(_, _) ->
    [].
