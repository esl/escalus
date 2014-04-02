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

-module(escalus_client).

% Public API
-export([start_for/3,
         start/3,
         send/2,
         send_and_wait/2,
         stop/1,
         kill/1,
         peek_stanzas/1, has_stanzas/1,
         wait_for_stanzas/2, wait_for_stanzas/3,
         wait_for_stanza/1, wait_for_stanza/2,
         is_client/1,
         full_jid/1,
         short_jid/1,
         username/1,
         server/1,
         resource/1]).

-import(escalus_compat, [bin/1, unimplemented/0]).

-define(WAIT_FOR_STANZA_TIMEOUT, 1000).

-include("include/escalus.hrl").
-include_lib("exml/include/exml.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

start(Config, UserSpec, Resource) ->
    EventClient = escalus_event:new_client(Config, UserSpec, Resource),
    Options = escalus_users:get_options(Config, UserSpec, Resource, EventClient),
    case escalus_connection:start(Options) of
        {ok, Conn, Props, _} ->
            Jid = make_jid(Props),
            Client = #client{jid = Jid, conn = Conn, event_client = EventClient},
            escalus_cleaner:add_client(Config, Client),
            {ok, Client};
        {error, Error} ->
            {error, Error}
    end.

start_for(Config, Username, Resource) ->
    %% due to escalus_client:get_user_option hack,
    %% those two are equivalent now
    start(Config, Username, Resource).

stop(#client{conn = Conn}) ->
    escalus_connection:stop(Conn).

kill(#client{conn = #transport{module = escalus_tcp, rcv_pid = Pid}}) ->
    erlang:exit(Pid, kill).

peek_stanzas(#client{conn = Conn}) ->
    {messages, Msgs} = process_info(self(), messages),
    lists:flatmap(fun ({stanza, MConn, Stanza}) when MConn == Conn ->
                          [Stanza];
                      %% FIXME: stream error
                      (_) ->
                          []
                 end, Msgs).

has_stanzas(Client) ->
    peek_stanzas(Client) /= [].

wait_for_stanzas(Client, Count) ->
    wait_for_stanzas(Client, Count, ?WAIT_FOR_STANZA_TIMEOUT).

wait_for_stanzas(Client, Count, Timeout) ->
    {ok, Tref} = timer:send_after(Timeout, self(), TimeoutMsg={timeout, make_ref()}),
    Result = do_wait_for_stanzas(Client, Count, TimeoutMsg, []),
    timer:cancel(Tref),
    Result.

do_wait_for_stanzas(_Client, 0, _TimeoutMsg, Acc) ->
    lists:reverse(Acc);
do_wait_for_stanzas(#client{conn = Conn, event_client=EventClient}=Client,
                    Count, TimeoutMsg, Acc) ->
    receive
        {stanza, Conn, Stanza} ->
            escalus_event:pop_incoming_stanza(EventClient, Stanza),
            do_wait_for_stanzas(Client, Count - 1, TimeoutMsg, [Stanza|Acc]);
        %% FIXME: stream error
        TimeoutMsg ->
            do_wait_for_stanzas(Client, 0, TimeoutMsg, Acc)
    end.

wait_for_stanza(Client) ->
    wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT).

wait_for_stanza(Client, Timeout) ->
    case wait_for_stanzas(Client, 1, Timeout) of
        [Stanza] ->
            Stanza;
        [] ->
            exit({timeout_when_waiting_for_stanza, Client})
    end.

send(#client{conn = Conn}, Packet) ->
    escalus_connection:send(Conn, Packet).

send_and_wait(Client, Packet) ->
    ok = send(Client, Packet),
    wait_for_stanza(Client).

is_client(#client{}) ->
    true;
is_client(_) ->
    false.

full_jid(#client{jid=Jid}) ->
    Jid.

short_jid(Client) ->
    escalus_utils:regexp_get(full_jid(Client), <<"^([^/]*)">>).

username(Client) ->
    escalus_utils:regexp_get(full_jid(Client), <<"^([^@]*)">>).

server(Client) ->
    escalus_utils:regexp_get(full_jid(Client), <<"^[^@]*[@]([^/]*)">>).

resource(Client) ->
    escalus_utils:regexp_get(full_jid(Client), <<"^[^/]*[/](.*)">>).

%%--------------------------------------------------------------------
%% helpers
%%--------------------------------------------------------------------

make_jid(Proplist) ->
    {username, U} = lists:keyfind(username, 1, Proplist),
    {server, S} = lists:keyfind(server, 1, Proplist),
    {resource, R} = lists:keyfind(resource, 1, Proplist),
    <<U/binary,"@",S/binary,"/",R/binary>>.
