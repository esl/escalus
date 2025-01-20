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
         story_with_client_list/3,
         make_everyone_friends/1,
         make_everyone_friends/2,
         make_all_clients_friends/1,
         start_ready_clients/2,
         send_initial_presence/1]).

-include("escalus.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

%% TODO: this doc sucks - what is "the standard format"?
%% @doc Run an test story using the standard format:
%%
%%   story(Config, [{alice, 2}, {bob, 1}], fun (Alice1, Alice2, Bob) ->
%%             ...
%%         end).
%%
%% @end
-spec story(escalus:config(), term(), fun()) -> term().
story(ConfigIn, ResourceCounts, Story) ->
    story(ConfigIn, ResourceCounts, Story, []).

%% @doc Run a story, but pass all the connected clients as a list.
%% This is needed when the number of resources is variable (pseudo-Erlang!):
%%
%%   story(Config, [{alice, 2}, {bob, N}], fun (Alice1, Alice2, Bob1, Bob2, ..., BobN) ->
%%             ...
%%         end).
%%
%% The closest we can get is:
%%
%%   story(Config, [{alice, 2}, {bob, N}], fun (Clients) ->
%%             [Alice1, Alice2, Bob1 | BobsUpToN] = Clients,
%%             ...
%%         end).
%%
%% See carboncopy_SUITE test properties for an example:
%% https://github.com/esl/MongooseIM/blob/f0ed90a93e17f7f3d5baf4d51bbb3f8b19826dd8/test.disabled/ejabberd_tests/tests/carboncopy_SUITE.erl#L183-L185
%% @end
-spec story_with_client_list(escalus:config(), term(), fun()) -> term().
story_with_client_list(ConfigIn, ResourceCounts, Story) ->
    story(ConfigIn, ResourceCounts, Story, [clients_as_list]).

-spec story(escalus:config(), term(), fun(), proplists:proplist()) -> term().
story(ConfigIn, ResourceCounts, Story, Opts) ->
    ClientDescs = clients_from_resource_counts(ConfigIn, ResourceCounts),
    try
        Config = escalus_server:pre_story(ConfigIn),
        Clients = start_clients(Config, ClientDescs),
        ensure_all_clean(Clients),
        escalus_event:story_start(Config),
        apply(Story, clients_as_arguments_or_list(Opts, Clients)),
        escalus_event:story_end(Config),
        post_story_checks(Config, Clients),
        escalus_server:post_story(Config),
        stop_clients(ConfigIn)
    catch Class:Reason:Stacktrace ->
        escalus_event:print_history(ConfigIn),
        erlang:raise(Class, Reason, Stacktrace)
    after
        kill_client_connections(ConfigIn)
    end.

-spec make_everyone_friends(escalus:config()) -> escalus:config().
make_everyone_friends(Config) ->
    Users = escalus_config:get_config(escalus_users, Config),
    make_everyone_friends(Config, Users).

-spec make_everyone_friends(escalus:config(), [{_, _}]) -> escalus:config().
make_everyone_friends(Config0, Users) ->
    % start the clients
    Config1 = escalus_cleaner:start(Config0),
    Clients = start_clients(Config1, [[{US, <<"friendly">>}] || {_Name, US} <- Users]),

    make_all_clients_friends(Clients),

    % stop the clients
    stop_clients(Config1),
    escalus_cleaner:stop(Config1),

    % return Config0
    [{everyone_is_friends, true} | Config0].

-spec make_all_clients_friends(Clients :: [escalus:client()]) -> ok.
make_all_clients_friends(Clients) ->
    % exchange subscribe and subscribed stanzas
    escalus_utils:distinct_pairs(fun(C1, C2) ->
        send_presence(C1, <<"subscribe">>, C2),
        swallow_stanzas(C1, 1, 0),
        swallow_stanzas(C2, 0, 1),
        send_presence(C2, <<"subscribe">>, C1),
        swallow_stanzas(C1, 1, 1),
        swallow_stanzas(C2, 1, 0),
        send_presence(C2, <<"subscribed">>, C1),
        swallow_stanzas(C1, 1, 2),
        swallow_stanzas(C2, 1, 0),
        send_presence(C1, <<"subscribed">>, C2),
        swallow_stanzas(C1, 1, 0),
        swallow_stanzas(C2, 1, 2)
    end, Clients),

    ensure_all_clean(Clients).

call_start_ready_clients(Config, UserCDs) ->
    escalus_overridables:do(Config, start_ready_clients, [Config, UserCDs],
                            {?MODULE, start_ready_clients}).

-spec start_ready_clients(escalus:config(), [term()]) -> [escalus:client()].
start_ready_clients(Config, FlatCDs) ->
    {_, RClients} = lists:foldl(fun({UserSpec, BaseResource}, {N, Acc}) ->
        Resource = escalus_overridables:do(Config, modify_resource, [BaseResource],
                                           {escalus_utils, identity}),
        {ok, Client} = escalus_client:start(Config, UserSpec, Resource),
        escalus_overridables:do(Config, initial_activity, [Client],
                                {?MODULE, send_initial_presence}),
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

-spec send_initial_presence(escalus:client()) -> ok.
send_initial_presence(Client) ->
    escalus_client:send(Client, escalus_stanza:presence(<<"available">>)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

send_presence(From, Type, To) ->
    ToJid = escalus_client:short_jid(To),
    Stanza = escalus_stanza:presence_direct(ToJid, Type),
    escalus_client:send(From, Stanza).

swallow_stanzas(User, NoRosters, NoPresences) ->
    Rosters = lists:duplicate(NoRosters, is_roster_set),
    Presences = lists:duplicate(NoPresences, is_presence),
    Stanzas = escalus:wait_for_stanzas(User, NoRosters + NoPresences),
    escalus:assert_many(Rosters ++ Presences, Stanzas).

ensure_all_clean(Clients) ->
    lists:foreach(fun(Client) ->
        escalus_assert:has_no_stanzas(Client)
    end, Clients).

start_clients(Config, ClientDescs) ->
    case proplists:get_bool(everyone_is_friends, Config) of
        true ->
            call_start_ready_clients(Config, lists:append(ClientDescs));
        false ->
            lists:flatmap(fun(UserCDs) ->
                call_start_ready_clients(Config, UserCDs)
            end, ClientDescs)
    end.

stop_clients(Config) ->
    Clients = escalus_cleaner:get_clients(Config),
    lists:foreach(fun(Client) -> escalus_client:stop(Config, Client) end, Clients).

kill_client_connections(Config) ->
    Clients = escalus_cleaner:get_clients(Config),
    lists:foreach(fun(Client) -> escalus_client:kill_connection(Config, Client) end, Clients).

drop_presences(Client, N) ->
    Dropped = escalus_client:wait_for_stanzas(Client, N),
    [escalus:assert(is_presence, Stanza) || Stanza <- Dropped],
    N = length(Dropped).

post_story_checks(Config, Clients) ->
    case proplists:get_bool(escalus_no_stanzas_after_story, Config) of
        true ->
            lists:foreach(
                fun escalus_assert:has_no_stanzas/1,
                Clients
            );
        _ ->
            ok
    end.

zip_shortest([H1|T1], [H2|T2]) ->
    [{H1,H2}|zip_shortest(T1, T2)];
zip_shortest(_, _) ->
    [].

%% ResourceCounts is a list of tuples: [{alice,2}, {bob,1}]
clients_from_resource_counts(Config, ResourceCounts = [{_, _} | _]) ->
    NamedSpecs = escalus_config:get_config(escalus_users, Config),
    [resources_per_spec(UserSpec, ResCount)
     || { User, ResCount} <- ResourceCounts,
        {_User, UserSpec} <- [lists:keyfind(User, 1, NamedSpecs)]];
%% Old-style ResourceCounts: [2, 1]
clients_from_resource_counts(Config, ResourceCounts) ->
    Deprecated = io_lib:format("DEPRECATED resource counts ~p (use [{alice, 1}, ...] or similar)",
                              [ResourceCounts]),
    escalus_compat:complain(Deprecated),
    NamedSpecs = escalus_config:get_config(escalus_users, Config),
    [resources_per_spec(UserSpec, ResCount)
     || {{_, UserSpec}, ResCount} <- zip_shortest(NamedSpecs,
                                                  ResourceCounts)].

resources_per_spec(UserSpec, ResCount) ->
    [{UserSpec, list_to_binary("res" ++ integer_to_list(N))}
     || N <- lists:seq(1, ResCount)].

clients_as_arguments_or_list(Opts, Clients) ->
    case proplists:get_value(clients_as_list, Opts, false) of
        false -> Clients;
        true -> [Clients]
    end.
