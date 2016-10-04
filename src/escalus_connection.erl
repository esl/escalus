%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module providing basic client functionality
%%% @end
%%%===================================================================
-module(escalus_connection).

-include("escalus.hrl").

%% High-level API
-export([start/1, start/2,
         stop/1]).

%% Low-level API
-export([connect/1,
         send/2,
         get_stanza/2,
         get_stanza/3,
         get_sm_h/1,
         set_sm_h/2,
         set_filter_predicate/2,
         reset_parser/1,
         is_connected/1,
         wait_for_close/2,
         kill/1]).

%% Behaviour helpers
-export([maybe_forward_to_owner/4]).

%% Public Types
-type client() :: #client{}.
-export_type([client/0]).

-type step_spec() :: atom() | {module(), atom()} | escalus_session:step().
-export_type([step_spec/0]).

-type filter_pred() :: fun((exml:element()) -> boolean()) | none.
-export_type([filter_pred/0]).

-export_type([t/0]).

%% Private
-export([connection_step/2]).

-define(TIMEOUT, 1000).

%%%===================================================================
%%% Behaviour callback
%%%===================================================================

%% When callback modules are referred to in specs,
%% they're escalus_connection:t().
%% This is purely informal, for Dialyzer it's just a module().
-type t() :: module().

-callback connect([proplists:property()]) -> {ok, client()}.
-callback send(client(), exml:element()) -> ok.
-callback stop(client()) -> ok | already_stopped.

-callback is_connected(client()) -> boolean().
-callback reset_parser(client()) -> ok.
-callback kill(client()) -> any().
-callback set_filter_predicate(client(), filter_pred()) -> ok.


%%%===================================================================
%%% Public API
%%%===================================================================

-spec start(escalus_users:user_spec()) -> {ok, client(), escalus_users:user_spec()}
                                        | {error, any()}.
start(Props) ->
    start(Props, get_connection_steps(Props)).

%% Usage:
%%
%%     start(Props,
%%           [start_stream,
%%            maybe_use_ssl,
%%            maybe_use_compression,
%%            authenticate,
%%            bind,
%%            session])
%%
%%     or
%%
%%     start(Props,
%%           [start_stream,
%%            authenticate,
%%            bind])
%%     or
%%
%%     start(Props, [start_stream])
%%
%% 'maybe_*' will check allowed properties and features to see if it's possible
%% to use a feature.
%% Others will assume a feature is available and fail if it's not.
-spec start(escalus_users:user_spec(),
            [step_spec()]) -> {ok, client(), escalus_users:user_spec()} |
                              {error, any()}.
start(Props0, Steps) ->
    try
        {ok, Conn, Props} = connect(Props0),
        {Conn1, Props1, Features} = lists:foldl(fun connection_step/2,
                                                {Conn, Props, []},
                                                [prepare_step(Step)
                                                 || Step <- Steps]),
        {ok, Conn1, Props1, Features}
    catch
        throw:{connection_step_failed, _Details, _Reason} = Error ->
            {error, Error}
    end.

connection_step(Step, {Conn, Props, Features}) ->
    try
        case Step of
            {Mod, Fun} ->
                apply(Mod, Fun, [Conn, Props, Features]);
            Fun ->
                apply(Fun, [Conn, Props, Features])
        end
    catch
        Error ->
            (Conn#client.module):stop(Conn),
            throw({connection_step_failed, {Step, Conn, Props, Features}, Error})
    end.

%% By default use predefined connection steps from escalus_session.
prepare_step(Step) when is_atom(Step) ->
    {escalus_session, Step};
%% Accept functions defined in other modules.
prepare_step({Mod, Fun}) when is_atom(Mod), is_atom(Fun) ->
    {Mod, Fun};
%% Accept funs of arity 3.
prepare_step(Fun) when is_function(Fun, 3) ->
    Fun.

-spec connect(escalus_users:user_spec()) -> {ok, client(), escalus_users:user_spec()}.
connect(Props) ->
    Transport = proplists:get_value(transport, Props, escalus_tcp),
    Server = proplists:get_value(server, Props, <<"localhost">>),
    Host = proplists:get_value(host, Props, Server),
    NewProps = lists:keystore(host, 1, Props, {host, Host}),
    {ok, Conn} = Transport:connect(NewProps),
    {ok, Conn, NewProps}.

-spec send(escalus:client(), exml:element()) -> ok.
send(#client{module = Mod, event_client = EventClient, jid = Jid} = Client, Elem) ->
    escalus_event:outgoing_stanza(EventClient, Elem),
    escalus_ct:log_stanza(Jid, out, Elem),
    Mod:send(Client, Elem).

-spec get_stanza(client(), any()) -> exml_stream:element().
get_stanza(Conn, Name) ->
    get_stanza(Conn, Name, ?TIMEOUT).

-spec get_stanza(client(), any(), timeout()) -> exml_stream:element().
get_stanza(#client{rcv_pid = Pid, jid = Jid}, Name, Timeout) ->
    receive
        {stanza, #client{rcv_pid = Pid}, Stanza} ->
            escalus_ct:log_stanza(Jid, in, Stanza),
            Stanza
    after Timeout ->
            throw({timeout, Name})
    end.

-spec get_sm_h(#client{}) -> non_neg_integer().
get_sm_h(#client{module = escalus_tcp} = Conn) ->
    escalus_tcp:get_sm_h(Conn);
get_sm_h(#client{module = Mod}) ->
    error({get_sm_h, {undefined_for_escalus_module, Mod}}).

-spec set_sm_h(#client{}, non_neg_integer()) -> non_neg_integer().
set_sm_h(#client{module = escalus_tcp} = Conn, H) ->
    escalus_tcp:set_sm_h(Conn, H);
set_sm_h(#client{module = Mod}, _) ->
    error({set_sm_h, {undefined_for_escalus_module, Mod}}).

-spec set_filter_predicate(client(), filter_pred()) -> ok.
set_filter_predicate(#client{module = Module} = Conn, Pred) ->
    Module:set_filter_predicate(Conn, Pred).

-spec reset_parser(client()) -> ok.
reset_parser(#client{module = Mod} = Client) ->
    Mod:reset_parser(Client).

-spec is_connected(client()) -> boolean().
is_connected(#client{module = Mod} = Client) ->
    Mod:is_connected(Client).

-spec stop(client()) -> ok | already_stopped.
stop(#client{module = Mod} = Client) ->
    Mod:stop(Client).

%% @doc Brutally kill the connection without terminating the XMPP stream.
-spec kill(client()) -> any().
kill(#client{module = Mod} = Client) ->
    Mod:kill(Client).

%% @doc Waits at most MaxWait ms for the client to be closed.
%% Returns true if the client was disconnected, otherwise false.
-spec wait_for_close(client(), non_neg_integer()) -> boolean().
wait_for_close(Client, MaxWait) ->
    %% Determine how many times the is_connect check should be run.
    %% There will be 100ms sleep between subsequent checks.
    %% This guarantees at least one check and no longer than MaxWait + 100ms
    NoOfTries = MaxWait div 100 + 1,
    do_wait_for_close(Client, NoOfTries).

do_wait_for_close(Client, 0) ->
    false == is_connected(Client);
do_wait_for_close(Client, N) ->
    case is_connected(Client) of
        false ->
            true;
        _ ->
            timer:sleep(100),
            do_wait_for_close(Client, N - 1)
    end.


-spec maybe_forward_to_owner(filter_pred(), term(), [exml:element()],
                             fun(([exml:element()], term()) -> term()))
        -> term().
maybe_forward_to_owner(none, State, _Stanzas, _Fun) ->
    State;
maybe_forward_to_owner(FilterPred, State, Stanzas, Fun)
    when is_function(FilterPred) ->
    AllowedStanzas = lists:filter(FilterPred, Stanzas),
    case AllowedStanzas of
        [] ->
            State;
        _ ->
            Fun(AllowedStanzas, State)
    end;
maybe_forward_to_owner(_, State, Stanzas, Fun) ->
    Fun(Stanzas, State).


%%%===================================================================
%%% Helpers
%%%===================================================================

get_connection_steps(UserSpec) ->
    case lists:keyfind(connection_steps, 1, UserSpec) of
        false -> default_connection_steps();
        {_, Steps} -> Steps
    end.

default_connection_steps() ->
    [start_stream,
     stream_features,
     maybe_use_ssl,
     authenticate,
     maybe_use_compression,
     bind,
     session,
     maybe_stream_management,
     maybe_use_carbons].
