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
-export([start_session/3,
         login/3,
         start_for/3,
         start/3,
         send/2,
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

-define(WAIT_FOR_STANZA_TIMOUT, 1000).

-include("escalus.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

start_session(Config, UserSpec, Resource) ->
    {server, Server} = proplists:lookup(server, UserSpec),
    Host = escalus_config:get_config(host, UserSpec, escalus_host, Config, Server),
    Port = escalus_config:get_config(port, UserSpec, escalus_port, Config, 5222),
    [Username, Server, Password] = escalus_users:get_usp(UserSpec),
    JID = exmpp_jid:make(Username, Server, Resource),

    Session = exmpp_session:start({1,0}),
    exmpp_session:auth_info(Session, JID, Password),
    case exmpp_session:connect_TCP(Session, Host, Port) of
        {ok, _, _} -> ok;
        {ok, _} -> ok
    end,
    Session.

login(Config, Session, UserSpec) ->
    AuthMethod = escalus_config:get_config(auth_method, UserSpec,
                                           escalus_auth_method, Config,
                                           "PLAIN"),
    {ok, _RealJid} = exmpp_session:login(Session, AuthMethod).

start(Config, UserSpec, Resource) ->
    Master = self(),
    ClientRef = make_ref(),
    {ClientPid, ClientMon} = spawn_monitor(fun() ->
        Session = escalus_overridables:do(Config, start_session,
                                          [Config, UserSpec, Resource],
                                          {?MODULE, start_session}),
        {ok, JID} = login(Config, Session, UserSpec),
        Master ! {logged_in, ClientRef, JID, Session},
        client_loop(ClientRef, Master)
    end),
    receive
        {logged_in, ClientRef, JID, Session} ->
            Client = #client{session=Session,
                             jid=JID#jid.raw,
                             pid=ClientPid,
                             ref=ClientRef},
            escalus_cleaner:add_client(Config, Client),
            {ok, Client};
        {'DOWN', ClientMon, _, _, Info} ->
            {error, {login_failed, UserSpec, Info}}
    end.

start_for(Config, Username, Resource) ->
    {Username, UserSpec} = escalus_users:get_user_by_name(Username),
    start(Config, UserSpec, Resource).

stop(#client{session=Session}) ->
    exmpp_session:stop(Session).

kill(#client{session=Session}) ->
    erlang:exit(Session, kill).

peek_stanzas(#client{ref=Ref}) ->
    {messages, Msgs} = process_info(self(), messages),
    lists:flatmap(fun ({got_stanza, MRef, Stanza}) when MRef == Ref ->
                          [Stanza];
                      ({stream_error, MRef, Reason}) when MRef == Ref ->
                          [{stream_error, Reason}];
                      (_) ->
                          []
                 end, Msgs).

has_stanzas(Client) ->
    peek_stanzas(Client) /= [].

wait_for_stanzas(Client, Count) ->
    wait_for_stanzas(Client, Count, ?WAIT_FOR_STANZA_TIMOUT).

wait_for_stanzas(Client, Count, Timeout) ->
    {ok, Tref} = timer:send_after(Timeout, self(), TimeoutMsg={timeout, make_ref()}),
    Result = do_wait_for_stanzas(Client, Count, TimeoutMsg, []),
    timer:cancel(Tref),
    Result.

do_wait_for_stanzas(_Client, 0, _TimeoutMsg, Acc) ->
    lists:reverse(Acc);
do_wait_for_stanzas(#client{ref=Ref}=Client, Count, TimeoutMsg, Acc) ->
    receive
        {got_stanza, Ref, Stanza} ->
            do_wait_for_stanzas(Client, Count - 1, TimeoutMsg, [Stanza|Acc]);
        {stream_error, Ref, Reason} ->
            do_wait_for_stanzas(Client, Count - 1, TimeoutMsg, [{stream_error, Reason}|Acc]);
        TimeoutMsg ->
            do_wait_for_stanzas(Client, 0, TimeoutMsg, Acc)
    end.

wait_for_stanza(Client) ->
    wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMOUT).

wait_for_stanza(Client, Timeout) ->
    case wait_for_stanzas(Client, 1, Timeout) of
        [Stanza] ->
            Stanza;
        [] ->
            exit({timeout_when_waiting_for_stanza, Client})
    end.

send(#client{session=Session}, Packet) ->
    exmpp_session:send_packet(Session, Packet).

is_client(#client{}) ->
    true;
is_client(_) ->
    false.

full_jid(#client{jid=Jid}) ->
    Jid.

short_jid(Client) ->
    regexp_get(full_jid(Client), <<"^([^/]*)">>).

username(Client) ->
    regexp_get(full_jid(Client), <<"^([^@]*)">>).

server(Client) ->
    regexp_get(full_jid(Client), <<"^[^@]*[@]([^/]*)">>).

resource(Client) ->
    regexp_get(full_jid(Client), <<"^[^/]*[/](.*)">>).

%%--------------------------------------------------------------------
%% spawn export
%%--------------------------------------------------------------------

client_loop(ClientRef, Master) ->
    receive
        #received_packet{raw_packet=Packet} ->
            Master ! {got_stanza, ClientRef, Packet},
            client_loop(ClientRef, Master);
        {stream_error, Reason} ->
            Master ! {stream_error, ClientRef, Reason},
            client_loop(ClientRef, Master);
        Other ->
            error_logger:error_msg("bad message: ~p~n", [Other]),
            client_loop(ClientRef, Master)
    end.

%%--------------------------------------------------------------------
%% helpers
%%--------------------------------------------------------------------

regexp_get(Jid, Regex) ->
    {match, [ShortJid]} =
        re:run(Jid, Regex, [{capture, all_but_first, binary}]),
    ShortJid.
