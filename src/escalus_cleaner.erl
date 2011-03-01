%%--------------------------------------------------------------------
%% Cleaner module.
%%
%% The cleaner should be started at the init of each test case.
%% Clients are registered to it (using add_client/2) as they are
%% started. In the end of test case stop/1 should be called which
%% stops all the registered clients.
%%--------------------------------------------------------------------
-module(escalus_cleaner).

% Public API
-export([start/1, add_client/2, stop/1]).

% spawn exports
-export([cleaner_loop/1]).

-include_lib("test_server/include/test_server.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

start(Config) ->
    Cleaner = spawn(?MODULE, cleaner_loop, [[]]),
    [{escalus_cleaner, Cleaner} | Config].

add_client(Config, Client) ->
    ?config(escalus_cleaner, Config) ! {add, Client}.

stop(Config) ->
    ?config(escalus_cleaner, Config) ! clean.

%%--------------------------------------------------------------------
%% cleaner
%%--------------------------------------------------------------------

cleaner_loop(ClientList) ->
    receive
        {add, NewClient} ->
            cleaner_loop([NewClient | ClientList]);
        clean ->
            lists:foreach(fun escalus_client:stop/1, ClientList)
    end.
