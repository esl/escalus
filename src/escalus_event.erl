-module(escalus_event).

%% Installation/deinstallation of the event mgr
-export([start/1,
         stop/1,
         new_client/3,
	     add_handler/3,
	     delete_handler/3]).

%% Notifications
-export([incoming_stanza/2,
         outgoing_stanza/2,
         pop_incoming_stanza/2]).

%% History
-export([get_history/1,
         print_history/1]).

-type manager() :: pid().

%% =======================================================================

%% @doc Add an event handler
%% @end
-spec add_handler(manager(), atom() | pid(), [term()]) -> ok.
add_handler(Mgr, Handler, Args) ->
    gen_event:add_handler(Mgr, Handler, Args).

%% @doc Delete an event handler
%% @end
-spec delete_handler(manager(), atom() | pid(), [term()]) -> ok.
delete_handler(Mgr, Handler, Args) ->
    gen_event:delete_handler(Mgr, Handler, Args).

incoming_stanza(Client, Stanza) ->
    notify_stanza(Client, incoming_stanza, Stanza).

pop_incoming_stanza(Client, Stanza) ->
    notify_stanza(Client, pop_incoming_stanza, Stanza).

outgoing_stanza(Client, Stanza) ->
    notify_stanza(Client, outgoing_stanza, Stanza).

%% ====================================================================

%% @doc Start the event manager
%% @end
start(Config) ->
    {ok, Mgr} = gen_event:start_link(),
    add_handler(Mgr, escalus_history_h, []),
    [{escalus_event_mgr, Mgr} | Config].

%% @doc Stop the event manager
%% @end
stop(Config) ->
    gen_event:stop(manager(Config)),
    Config.

get_history(Config) ->
    escalus_history_h:get_history(manager(Config)).

print_history(Config) ->
    Events = get_history(Config),
    io:format("~p", [Events]),
    ok.

manager(Config) ->
    proplists:get_value(escalus_event_mgr, Config).

%% @doc Create a new event emitter.
new_client(Config, UserSpec, Resource) ->
    [{event_manager, manager(Config)},
     {server, proplists:get_value(server, UserSpec)},
     {username, proplists:get_value(username, UserSpec)},
     {resource, Resource}].

%% @doc Notify the event system of an event
%% <p>The system accepts any term as the event.</p>
%% @end
notify_stanza(undefined, _, _) ->
    ok;
notify_stanza(Client, EventName, Stanza) ->
    Mgr = proplists:get_value(event_manager, Client),
    Jid = jid(Client),
    gen_event:notify(Mgr, {EventName, Jid, Stanza}).


jid(Client) ->
    Server   = proplists:get_value(server, Client),
    User     = proplists:get_value(username, Client),
    Resource = proplists:get_value(server, Client),
    {User, Server, Resource}.
