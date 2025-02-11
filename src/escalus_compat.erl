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
%% Helper module to ease porting Escalus from exmpp to lxmppc
%%--------------------------------------------------------------------

-module(escalus_compat).

%% Public API
-export([bin/1,
         deprecated/3,
         unimplemented/0,
         complain/1,
         backtrace/1]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

-spec bin(binary() | string() | atom() | integer()) -> binary() | no_return().
bin(Arg) when is_binary(Arg) ->
    Arg;
bin(Arg) when is_list(Arg) ->
    type_complain("string", Arg),
    list_to_binary(Arg);
bin(Arg) when is_atom(Arg) ->
    type_complain("atom", Arg),
    list_to_binary(atom_to_list(Arg));
bin(Arg) when is_integer(Arg) ->
    type_complain("integer", Arg),
    integer_to_binary(Arg);
bin(Other) ->
    type_complain("???", Other),
    error(badarg, [Other]).

-spec deprecated(atom(), atom(), T) -> T.
deprecated(Old, New, Result) ->
    error_logger:info_msg("calling deprecated function ~p, use ~p instead~n~p~n",
                          [Old, New, backtrace(1)]),
    Result.

-spec unimplemented() -> no_return().
unimplemented() ->
    throw({unimplemented, backtrace(1)}).

-spec complain(term()) -> ok.
complain(What) ->
    error_logger:info_msg("~s at ~p~n", [What, backtrace(1)]).

-spec backtrace(non_neg_integer()) -> list().
backtrace(N) ->
    {current_stacktrace, Stacktrace} = erlang:process_info(self(), current_stacktrace),
    lists:nthtail(N + 1, Stacktrace).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

type_complain(Type, Value) ->
    error_logger:info_msg("expecting binary, got ~s ~p at ~p~n",
                          [Type, Value, backtrace(2)]).
