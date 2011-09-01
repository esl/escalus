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
-export([start/1, add_client/2, clean/1, stop/1]).

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

clean(Config) ->
    ?config(escalus_cleaner, Config) ! clean.

stop(Config) ->
    ?config(escalus_cleaner, Config) ! stop.

%%--------------------------------------------------------------------
%% cleaner
%%--------------------------------------------------------------------

cleaner_loop(ClientList) ->
    receive
        {add, NewClient} ->
            cleaner_loop([NewClient | ClientList]);
        clean ->
            lists:foreach(fun escalus_client:stop/1, ClientList),
            cleaner_loop([]);
        stop ->
            stop
    end.
