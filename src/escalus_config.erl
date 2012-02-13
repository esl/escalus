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
-export([get_config/2,
         get_config/3,
         get_config/5]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

get_config(Option, Config) ->
    get_config(Option, Config, undefined).

get_config(Option, Config, Default) ->
    case lists:keyfind(Option, 1, Config) of
        {Option, Value} ->
            Value;
        false ->
            case ct:get_config(Option) of
                undefined ->
                    Default;
                Value ->
                    Value
            end
    end.

get_config(USName, UserSpec, CName, Config, Default) ->
    case lists:keysearch(USName, 1, UserSpec) of
        {value, Value} ->
            Value;
        false ->
            get_config(CName, Config, Default)
    end.
