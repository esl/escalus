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

-module(escalus_new_assert).

%% This module is meant to replace legacy escalus_assert in future versions
%% of Escalus

-export([mix_match/2]).

mix_match(Predicates, Stanzas) ->
    case escalus_utils:mix_match(fun(F) -> {escalus_pred, F} end, Predicates, Stanzas) of
        true ->
            ok;
        false ->
            StanzasStr = escalus_utils:pretty_stanza_list(Stanzas),
            exit({assertion_failed, mix_match, Predicates, StanzasStr})
    end.
