%%==============================================================================
%% Copyright 2017 Erlang Solutions Ltd.
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

%% This module contains predicates (boolean value - returning functions)
%% That are meant to be used primarly with escalus:assert function

-module(escalus_pubsub_pred).

-export([
         is_item_retract/3,
         is_config_event/3
        ]).

-include("escalus.hrl").
-include("escalus_xmlns.hrl").
-include_lib("exml/include/exml_stream.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

is_item_retract({NodeAddr, NodeName}, ItemId, Stanza) ->
    HasValidPayload =
    case exml_query:subelements(Stanza, <<"event">>) of
        [#xmlel{ attrs = [{<<"xmlns">>, ?NS_PUBSUB_EVENT}] } = Event] ->
            case exml_query:subelements(Event, <<"items">>) of
                [#xmlel{ attrs = [{<<"node">>, NodeName}] } = Items] ->
                    case exml_query:subelements(Items, <<"retract">>) of
                        [#xmlel{ attrs = [{<<"id">>, ItemId}] }] ->
                            true;
                        _ ->
                            false
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end,
    escalus_pred:is_message(Stanza)
    andalso NodeAddr == exml_query:attr(Stanza, <<"from">>)
    andalso HasValidPayload.

is_config_event({NodeAddr, NodeName}, ConfigChange, Stanza) ->
    ValidOptionsChanged =
    case exml_query:subelements(Stanza, <<"event">>) of
        [#xmlel{ attrs = [{<<"xmlns">>, ?NS_PUBSUB_EVENT}] } = Event] ->
            case exml_query:subelements(Event, <<"configuration">>) of
                [#xmlel{ attrs = [{<<"node">>, NodeName}] } = ConfigEl] ->
                    Fields = exml_query:paths(ConfigEl, [{element, <<"x">>},
                                                         {element, <<"field">>}]),
                    Opts = [ {exml_query:attr(F, <<"var">>),
                              exml_query:path(F, [{element, <<"value">>}, cdata])} || F <- Fields ],
                    lists:all(fun({K, V}) ->
                                      {K, V} =:= lists:keyfind(K, 1, Opts)
                              end, ConfigChange);
                _ ->
                    false
            end;
        _ ->
            false
    end,
    escalus_pred:is_message(Stanza)
    andalso NodeAddr == exml_query:attr(Stanza, <<"from">>)
    andalso ValidOptionsChanged.

