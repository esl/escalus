%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc lxmppc application
%%% @end
%%%===================================================================
-module(lxmppc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    lxmppc_sup:start_link().

stop(_State) ->
    ok.
