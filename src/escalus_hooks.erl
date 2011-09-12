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

-module(escalus_hooks).

-export([run/3, run_default/4]).

-spec run([{atom(), term()}], atom(), [term()]) -> term().
run(Config, HookName, Args) ->
    case get_hook(Config, HookName, undefined) of
        undefined ->
            no_hook;
        {Mod, Fun} ->
            apply(Mod, Fun, Args)
    end.

-spec run_default([{atom(), term()}], atom(), [term()], {atom(), atom()}) -> term().
run_default(Config, HookName, Args, Default) ->
    {Mod, Fun} = get_hook(Config, HookName, Default),
    apply(Mod, Fun, Args).

%%==============================================================================
%% Helpers
%%==============================================================================

get_hook(Config, HookName, Default) ->
    case ct:get_config(escalus_hooks) of
        undefined ->
            Default;
        Hooks ->
            proplists:get_value(HookName, Hooks, Default)
    end.
