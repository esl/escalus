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
         stop/2,
         wait_for_close/3,
         kill_connection/2,
         kill/1,
         peek_stanzas/1, has_stanzas/1,
         wait_for_stanzas/2, wait_for_stanzas/3,
         wait_for_stanza/1, wait_for_stanza/2,
         send_iq_and_wait_for_result/2, send_iq_and_wait_for_result/3,
         is_client/1,
         full_jid/1,
         short_jid/1,
         username/1,
         server/1,
         resource/1
        ]).

-export_type([client/0]).

-define(WAIT_FOR_STANZA_TIMEOUT, 5000).

-include("escalus.hrl").
-include_lib("exml/include/exml.hrl").

-type client() :: #client{}.

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

-spec start(escalus:config(), escalus_users:user_spec(), binary()) -> {ok, _}
                                                                    | {error, _}.
start(Config, UserSpec, Resource) ->
    EventClient = escalus_event:new_client(Config, UserSpec, Resource),
    Options = escalus_users:get_options(Config, UserSpec, Resource, EventClient),
    case escalus_connection:start(Options) of
        {ok, Conn, _} ->
            Client = Conn#client{event_client = EventClient},
            escalus_cleaner:add_client(Config, Client),
            {ok, Client};
        {error, Error} ->
            {error, Error}
    end.

-spec start_for(escalus:config(), escalus_users:user_spec(), binary()) -> {ok, _}
                                                                        | {error, _}.
start_for(Config, UserSpec, Resource) ->
    %% due to escalus_client:get_user_option hack,
    %% those two are equivalent now
    start(Config, UserSpec, Resource).

-spec stop(escalus:config(), client()) -> ok.
stop(Config, Client) ->
    escalus_connection:stop(Client),
    escalus_cleaner:remove_client(Config, Client).

-spec kill_connection(escalus:config(), client()) -> ok.
kill_connection(Config, Client) ->
    escalus_connection:kill(Client),
    escalus_cleaner:remove_client(Config, Client).

-spec wait_for_close(escalus:config(), client(), non_neg_integer()) -> ok.
wait_for_close(Config, Client, Timeout) ->
    true = escalus_connection:wait_for_close(Client, Timeout),
    escalus_cleaner:remove_client(Config, Client).

-spec kill(client()) -> term().
kill(#client{module = escalus_tcp, rcv_pid = Pid}) ->
    erlang:exit(Pid, kill).

-spec peek_stanzas(client()) -> [exml:element()].
peek_stanzas(#client{rcv_pid = Pid}) ->
    {messages, Msgs} = process_info(self(), messages),
    lists:flatmap(fun ({stanza, StanzaPid, Stanza, _}) when Pid == StanzaPid ->
                          [Stanza];
                      %% FIXME: stream error
                      (_) ->
                          []
                 end, Msgs).

-spec has_stanzas(client()) -> boolean().
has_stanzas(Client) ->
    peek_stanzas(Client) /= [].

-spec wait_for_stanzas(client(), non_neg_integer()) -> [exml:element()].
wait_for_stanzas(Client, Count) ->
    wait_for_stanzas(Client, Count, ?WAIT_FOR_STANZA_TIMEOUT).

-spec wait_for_stanzas(client(), non_neg_integer(), non_neg_integer()) -> [exml:element()].
wait_for_stanzas(Client, Count, Timeout) ->
    do_wait_for_stanzas(Client, Count, Timeout, []).

do_wait_for_stanzas(_Client, 0, _TimeoutMsg, Acc) ->
    lists:reverse(Acc);
do_wait_for_stanzas(#client{event_client=EventClient, jid=Jid, rcv_pid=Pid} = Client,
                    Count, Timeout, Acc) ->
    case escalus_connection:get_stanza_safe(Client, Timeout) of
        {error,  timeout} ->
            do_wait_for_stanzas(Client, 0, Timeout, Acc);
        {Stanza, _} ->
            escalus_event:pop_incoming_stanza(EventClient, Stanza),
            do_wait_for_stanzas(Client, Count - 1, Timeout, [Stanza|Acc])
        %% FIXME: stream error
    end.

-spec wait_for_stanza(client()) -> exml:element().
wait_for_stanza(Client) ->
    wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT).

-spec wait_for_stanza(client(), non_neg_integer()) -> exml:element().
wait_for_stanza(Client, Timeout) ->
    case wait_for_stanzas(Client, 1, Timeout) of
        [Stanza] ->
            Stanza;
        [] ->
            error(timeout_when_waiting_for_stanza, [Client, Timeout])
    end.

-spec send(client(), exml:element()) -> ok.
send(Client, Packet) ->
    escalus_connection:send(Client, Packet).

-spec send_and_wait(client(), exml:element()) -> exml:element().
send_and_wait(Client, Packet) ->
    ok = send(Client, Packet),
    wait_for_stanza(Client).

-spec send_iq_and_wait_for_result(client(), exml:element()) -> exml:element() | no_return().
send_iq_and_wait_for_result(Client, Iq) ->
    send_iq_and_wait_for_result(Client, Iq, ?WAIT_FOR_STANZA_TIMEOUT).

-spec send_iq_and_wait_for_result(client(), exml:element(), non_neg_integer()) ->
    exml:element() | no_return().
send_iq_and_wait_for_result(Client, #xmlel{name = <<"iq">>} = Req, Timeout) ->
    ok = send(Client, Req),
    Resp = #xmlel{name = RespName} = wait_for_stanza(Client, Timeout),
    RespType = exml_query:attr(Resp, <<"type">>, undefined),
    RespId = exml_query:attr(Resp, <<"id">>),
    ReqId = exml_query:attr(Req, <<"id">>),
    case {RespName, RespType, RespId == ReqId} of
        {<<"iq">>, <<"result">>, true} ->
            Resp;
        {<<"iq">>, <<"result">>, false} ->
            raise_invalid_iq_resp_error(received_invalid_iq_result_id, ReqId, RespId, Req, Resp);
        {<<"iq">>, _, _} ->
            raise_invalid_iq_resp_error(received_invalid_iq_stanza_type, <<"result">>, RespType,
                                        Req, Resp);
        {_, _, _} ->
            raise_invalid_iq_resp_error(received_invalid_stanza, <<"iq">>, RespName, Req, Resp)
    end.

-spec raise_invalid_iq_resp_error(atom(), term(), term(), exml:element(), exml:element()) ->
    no_return().
raise_invalid_iq_resp_error(Reason, Expected, Received, Req, Resp) ->
    error({Reason, [{expected, Expected},
                    {received, Received},
                    {request, Req},
                    {response, Resp}]}).

-spec is_client(term()) -> boolean().
is_client(#client{}) ->
    true;
is_client(_) ->
    false.

-spec full_jid(client()) -> binary() | undefined.
full_jid(#client{jid=Jid}) ->
    Jid.

-spec short_jid(client()) -> binary().
short_jid(Client) ->
    escalus_utils:regexp_get(full_jid(Client), <<"^([^/]*)">>).

-spec username(client()) -> binary().
username(Client) ->
    escalus_utils:regexp_get(full_jid(Client), <<"^([^@]*)">>).

-spec server(client()) -> binary().
server(Client) ->
    escalus_utils:regexp_get(full_jid(Client), <<"^[^@]*[@]([^/]*)">>).

-spec resource(client()) -> binary().
resource(Client) ->
    escalus_utils:get_resource(full_jid(Client)).

