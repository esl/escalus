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

-module(escalus_assert).

-export([is_chat_message/2, has_no_stanzas/1, is_iq/2]).

is_chat_message(Msg, Stanza) when is_list(Msg) ->
    is_chat_message(list_to_binary(Msg), Stanza);
is_chat_message(Msg, Stanza) when is_binary(Msg) ->
    chat = exmpp_message:get_type(Stanza),
    Msg = exmpp_message:get_body(Stanza).

is_iq(Type, Stanza) ->
    Type = exmpp_iq:get_type(Stanza).

has_no_stanzas(Client) ->
    false = escalus_client:has_stanzas(Client).
