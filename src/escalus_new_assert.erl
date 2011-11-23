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

-export([assert/2, assert/3, mix_match/2]).

%%==============================================================================
%% API functions
%%==============================================================================

assert(PredSpec, Arg) ->
    Fun = predspec_to_fun(PredSpec),
    StanzaStr = exmpp_xml:document_to_list(Arg),
    assert_true(Fun(Arg),
        {assertion_failed, assert, PredSpec, Arg, StanzaStr}).

assert(PredSpec, Params, Arg) ->
    Fun = predspec_to_fun(PredSpec),
    StanzaStr = exmpp_xml:document_to_list(Arg),
    assert_true(apply(Fun, Params ++ [Arg]),
        {assertion_failed, assert, PredSpec, Params, Arg, StanzaStr}).

mix_match(Predicates, Stanzas) ->
    AllStanzas = length(Predicates) == length(Stanzas),
    Ok = escalus_utils:mix_match(fun predspec_to_fun/1, Predicates, Stanzas),
    StanzasStr = escalus_utils:pretty_stanza_list(Stanzas),
    assert_true(Ok and AllStanzas,
        {assertion_failed, mix_match, Predicates, Stanzas, StanzasStr}).

%%==============================================================================
%% Helpers
%%==============================================================================

predspec_to_fun(F) when is_atom(F) ->
    {escalus_pred, F};
predspec_to_fun(Other) ->
    Other.

assert_true(true, _) -> ok;
assert_true(false, Fail) ->
    exit(Fail);
assert_true(WTF, Pred) ->
    exit({wtf, bad_predicate_value, WTF, Pred}).
