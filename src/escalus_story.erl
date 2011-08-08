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
-export([story/3]).

-include_lib("test_server/include/test_server.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

story(Config, ResourceCounts, Story) ->
    {escalus_users, NamedSpecs} = proplists:lookup(escalus_users, Config),
    Clients = [start_clients(Config, UserSpec, ResCount) ||
               {{_, UserSpec}, ResCount} <- zip_shortest(NamedSpecs,
                                                         ResourceCounts)],
    ClientList = lists:flatten(Clients),
    prepare_clients(Config, zip_shortest(NamedSpecs, Clients), length(ClientList)),
    apply(Story, ClientList),
    post_story_checks(Config, ClientList).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_clients(Config, UserSpec, ResourceCount) ->
    [start_client(Config, UserSpec, ResNo) ||
     ResNo <- lists:seq(1, ResourceCount)].

start_client(Config, UserSpec, ResNo) ->
    Res = "res" ++ integer_to_list(ResNo),
    escalus_client:start(Config, UserSpec, Res).

prepare_clients(Config, SpecsAndClients, TotalClients) ->
    do_when(Config, escalus_save_initial_history, false,
            % drop initial presences form all of the resources
            fun ({NamedSpec, Clients}) ->
                N = initial_stanza_count(Config, NamedSpec, Clients, TotalClients),
                lists:foreach(fun(Client) ->
                    Dropped = escalus_client:wait_for_stanzas(Client, N),
                    N = length(Dropped)
                end, Clients)
            end, SpecsAndClients).

initial_stanza_count(Config, {_UserName, UserSpec}, Clients, TotalClients) ->
    GetConfig = fun(Name) ->
                    escalus_config:get_config(Name, Config,
                                              Name, UserSpec,
                                              default_stanza)
                end,
    TotalCount =
        case GetConfig(initial_roster_get) of
            none -> 0;
            _Some -> 1
        end
        +
        case {GetConfig(initial_presence), GetConfig(everyone_is_friends)} of
            {none, _} -> 0; % no initial presence
            {_, true} -> TotalClients; % initial presence from everybody
            {_, _} -> length(Clients) % initial presence only from own resources
        end,
    TotalCount.


post_story_checks(Config, ClientList) ->
    do_when(Config, escalus_no_stanzas_after_story, true,
            fun escalus_assert:has_no_stanzas/1, ClientList).

do_when(Config, Key, Value, Fun, ClientList) ->
    case proplists:get_bool(Key, Config) of
        Value ->
            lists:foreach(Fun, ClientList);
        _ ->
            do_nothing
    end.

zip_shortest([H1|T1], [H2|T2]) ->
    [{H1,H2}|zip_shortest(T1, T2)];
zip_shortest(_, _) ->
    [].
