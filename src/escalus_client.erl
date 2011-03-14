-module(escalus_client).

% Public API
-export([start_session/3,
         login/3,
         start_for/3, start_for_wait/3,
         start/3, start_wait/3,
         send/2, send_wait/2,
         stop/1, stop_wait/1,
         kill/1, kill_wait/1,
         drop_history/1,
         peek_stanzas/1,
         get_stanzas/1,
         only_stanza/1,
         is_client/1]).

% spawn exports
-export([client_loop/2]).

-define(WAIT_TIME, 100).
-define(PEEK_DATA_TIMEOUT, 10000).

-include("escalus.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

start_session(Config, UserSpec, Resource) ->
    {server, Server} = proplists:lookup(server, UserSpec),
    Host = get_config(host, UserSpec, escalus_host, Config, Server),
    Port = get_config(port, UserSpec, escalus_port, Config, 5222),
    [Username, Server, Password] = escalus_users:get_usp(UserSpec),
    JID = exmpp_jid:make(Username, Server, Resource),

    Session = exmpp_session:start({1,0}),
    exmpp_session:auth_info(Session, JID, Password),
    {ok, _, _} = exmpp_session:connect_TCP(Session, Host, Port),
    Session.

login(Config, Session, UserSpec) ->
    AuthMethod = get_config(auth_method, UserSpec,
                            escalus_auth_method, Config,
                            "PLAIN"),
    {ok, _RealJid} = exmpp_session:login(Session, AuthMethod).

start(Config, UserSpec, Resource) ->
    Session = start_session(Config, UserSpec, Resource),
    {ok, JID} = login(Config, Session, UserSpec),
    ClientPid = spawn(?MODULE, client_loop, [Session, []]),
    exmpp_session:set_controlling_process(Session, ClientPid),
    copy_packet_messages(ClientPid),
    Client = #client{jid=JID#jid.raw, pid=ClientPid},
    escalus_cleaner:add_client(Config, Client),
    send_initial_presence(Config, UserSpec, Client),
    Client.

start_wait(Config, UserSpec, Resource) ->
    Client = start(Config, UserSpec, Resource),
    wait(),
    Client.

start_for(Config, Username, Resource) ->
    {Username, UserSpec} = escalus_users:get_user_by_name(Username),
    start(Config, UserSpec, Resource).

start_for_wait(Config, Username, Resource) ->
    Client = start_for(Config, Username, Resource),
    wait(),
    Client.

stop(#client{pid=Pid}) ->
    Pid ! stop.

stop_wait(Client) ->
    stop(Client),
    wait().

kill(#client{pid=Pid}) ->
    Pid ! kill.

kill_wait(Client) ->
    Client ! kill.

drop_history(#client{pid=Pid}) ->
    Pid ! drop_history.

peek_stanzas(#client{pid=Pid}) ->
    Pid ! {peek_stanzas, Ref=make_ref(), self()},
    receive
        {data, Ref, Data} ->
            Data
    after ?PEEK_DATA_TIMEOUT ->
            erlang:error(no_connection)
    end.

get_stanzas(Client) ->
    Stanzas = peek_stanzas(Client),
    drop_history(Client),
    Stanzas.

only_stanza(Client) ->
    [Stanza] = get_stanzas(Client),
    Stanza.

send(#client{pid=Pid}, Packet) ->
    Pid ! {send, Packet}.

send_wait(Client, Packet) ->
    send(Client, Packet),
    wait().

wait() ->
    timer:sleep(?WAIT_TIME).

is_client(#client{}) ->
    true;
is_client(_) ->
    false.

%%--------------------------------------------------------------------
%% spawn export
%%--------------------------------------------------------------------

client_loop(Session, Acc) ->
    receive
        stop ->
            exmpp_session:stop(Session);
        kill ->
            erlang:exit(Session, kill);
        drop_history ->
            client_loop(Session, []);
        {peek_stanzas, Ref, Pid} ->
            Pid ! {data, Ref, lists:reverse(Acc)},
            client_loop(Session, Acc);
        {send, Packet} ->
            exmpp_session:send_packet(Session, Packet),
            client_loop(Session, Acc);
        #received_packet{raw_packet=Packet} ->
            client_loop(Session, [Packet | Acc]);
        Other ->
            error_logger:error_msg("bad message: ~p~n", [Other])
    end.

%%--------------------------------------------------------------------
%% helpers
%%--------------------------------------------------------------------

send_initial_presence(Config, UserSpec, Client) ->
    case get_config(initial_presence, Config,
                    initial_presence, UserSpec,
                    exmpp_presence:available()) of
        none ->
            ok;
        Presence ->
            send_wait(Client, Presence)
    end.

copy_packet_messages(TargetPid) ->
    receive
        #received_packet{} = Packet ->
            error_logger:info_msg("got packet: ~p~n", [Packet]),
            TargetPid ! Packet,
            copy_packet_messages(TargetPid)
    after 0 ->
        done
end.

%%--------------------------------------------------------------------
%% utilities
%%--------------------------------------------------------------------

get_config(USName, UserSpec, CName, Config, Default) ->
    case proplists:get_value(USName, UserSpec, Missing=make_ref()) of
        Missing ->
            proplists:get_value(CName, Config, Default);
        Found ->
            Found
    end.
