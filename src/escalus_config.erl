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

-module(escalus_config).

%% Public API
-export([get_property/2,
         get_usp/1,
         get_config/5]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

get_property(Name, Config) ->
    {Name, Value} = proplists:lookup(Name, Config),
    Value.

get_usp(UserSpec) ->
    escalus_users:get_usp(UserSpec).

get_config(USName, UserSpec, CName, Config, Default) ->
    case proplists:get_value(USName, UserSpec, Missing=make_ref()) of
        Missing ->
            proplists:get_value(CName, Config, Default);
        Found ->
            Found
    end.
