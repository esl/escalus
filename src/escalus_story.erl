-module(escalus_story).

% Public API
-export([story/3]).

-include_lib("test_server/include/test_server.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

story(Config, ResourceCounts, Story) ->
    UserSpecs = escalus_users:get_users(all),
    Clients = [start_clients(Config, UserSpec, ResCount) ||
               {{_, UserSpec}, ResCount} <- zip_shortest(UserSpecs,
                                                         ResourceCounts)],
    ClientList = lists:flatten(Clients),
    prepare_clients(Config, ClientList),
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
    escalus_client:start_wait(Config, UserSpec, Res).

prepare_clients(Config, ClientList) ->
    do_when(Config, escalus_save_initial_history, false,
            fun escalus_client:drop_history/1, ClientList).

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
