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
-export([start/1,
         add_client/2,
         remove_client/2,
         get_clients/1,
         stop/1]).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
        clients = [] :: [pid()]
}).

-type state() :: #state{}.

%%%===================================================================
%%% Public API
%%%===================================================================

-spec start(escalus:config()) -> escalus:config().
start(Config) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    [{escalus_cleaner, Pid} | Config].

-spec add_client(escalus:config(), escalus:client()) -> ok.
add_client(Config, Client) ->
    gen_server:call(get_cleaner(Config), {add_client, Client}).

-spec remove_client(escalus:config(), escalus:client()) -> ok.
remove_client(Config, Client) ->
    gen_server:call(get_cleaner(Config), {remove_client, Client}).

-spec get_clients(escalus:config()) -> [escalus:client()].
get_clients(Config) ->
    gen_server:call(get_cleaner(Config), get_clients).

-spec stop(escalus:config()) -> ok.
stop(Config) ->
    gen_server:cast(get_cleaner(Config), stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(list()) -> {ok, state()}.
init([]) ->
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}
                                                           | {noreply, state()}
                                                           | {stop, normal, ok, state()}.
handle_call({add_client, Client}, _From, #state{clients = Clients} = State) ->
    {reply, ok, State#state{clients = [Client | Clients]}};
handle_call({remove_client, Client}, _From, #state{clients = Clients} = State) ->
    {reply, ok, State#state{clients = Clients -- [Client]}};
handle_call(get_clients, _From, #state{clients = Clients} = State) ->
    {reply, Clients, State}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {stop, normal, state()}.
handle_cast(stop, State) ->
    {stop, normal, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> term().
terminate(_Reason, #state{clients = []}) ->
    ok;
terminate(_Reason, #state{clients = Clients}) ->
    error_logger:warning_msg("cleaner finishes dirty: ~p~n", [Clients]),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% helpers
%%%===================================================================

get_cleaner(Config) ->
    {escalus_cleaner, Pid} = lists:keyfind(escalus_cleaner, 1, Config),
    Pid.
