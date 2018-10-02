%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module providing basic client functionality
%%% @end
%%%===================================================================
-module(escalus_connection).

-include_lib("exml/include/exml_stream.hrl").
-include("escalus.hrl").

%% High-level API
-export([start/1, start/2,
         stop/1]).

%% Low-level API
-export([connect/1,
         maybe_set_jid/1,
         send/2,
         get_stanza/2,
         get_stanza/3,
         get_stanza_safe/2,
         get_stanza_with_metadata/3,
         get_sm_h/1,
         set_sm_h/2,
         set_filter_predicate/2,
         reset_parser/1,
         is_connected/1,
         wait_for_close/2,
         kill/1,
         use_zlib/1,
         upgrade_to_tls/1,
         start_stream/1]).

-export([stanza_msg/2]).

%% Behaviour helpers
-export([maybe_forward_to_owner/5]).

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

-define(TIMEOUT, 5000).

%%%===================================================================
%%% Behaviour callback
%%%===================================================================

%% When callback modules are referred to in specs,
%% they're escalus_connection:t().
%% This is purely informal, for Dialyzer it's just a module().
-type t() :: module().

-callback connect([proplists:property()]) -> pid().
-callback send(pid(), exml:element()) -> ok.
-callback stop(pid()) -> ok | already_stopped.

-callback is_connected(pid()) -> boolean().
-callback reset_parser(pid()) -> ok.
-callback kill(pid()) -> ok | already_stopped.
-callback use_zlib(pid()) -> ok.
-callback upgrade_to_tls(pid(), proplists:proplist()) -> ok.
-callback set_filter_predicate(pid(), filter_pred()) -> ok.

-type stanza_msg() :: {stanza, pid(), exml:element(), map()}.

-type metadata() :: #{recv_timestamp := integer()}.

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
            [step_spec()]) -> {ok, client(), escalus_session:features()} |
                              {error, any()}.
start(Props, Steps) ->
    try
        Client = connect(Props),
        {Client1, Features} = lists:foldl(fun connection_step/2,
                                                {Client, []},
                                                [prepare_step(Step)
                                                 || Step <- Steps]),
        {ok, Client1, Features}
    catch
        throw:{connection_step_failed, _Details, _Reason} = Error ->
            {error, Error}
    end.

-spec connection_step(step_spec(), {client(), escalus_session:features()}) ->
                             {client(), escalus_session:features()}.
connection_step(Step, {Client, Features}) ->
    try
        case Step of
            {Mod, Fun} ->
                apply(Mod, Fun, [Client, Features]);
            Fun ->
                apply(Fun, [Client, Features])
        end
    catch
        Error ->
            kill(Client),
            throw({connection_step_failed, {Step, Client, Features}, Error})
    end.

%% By default use predefined connection steps from escalus_session.
prepare_step(Step) when is_atom(Step) ->
    {escalus_session, Step};
%% Accept functions defined in other modules.
prepare_step({Mod, Fun}) when is_atom(Mod), is_atom(Fun) ->
    {Mod, Fun};
%% Accept funs of arity 2.
prepare_step(Fun) when is_function(Fun, 2) ->
    Fun.

-spec connect(escalus_users:user_spec()) -> client().
connect(Props) ->
    Transport = proplists:get_value(transport, Props, escalus_tcp),
    Server = proplists:get_value(server, Props, <<"localhost">>),
    Host = proplists:get_value(host, Props, Server),
    NewProps = lists:keystore(host, 1, Props, {host, Host}),
    Pid = Transport:connect(NewProps),
    maybe_set_jid(#client{module = Transport, rcv_pid = Pid, props = NewProps}).

-spec maybe_set_jid(client()) -> client().
maybe_set_jid(Client = #client{props = Props}) ->
    case {lists:keyfind(username, 1, Props),
          lists:keyfind(server, 1, Props),
          lists:keyfind(resource, 1, Props)} of
        {{username, U}, {server, S}, false} ->
            Client#client{jid = <<U/binary, "@", S/binary>>};
        {{username, U}, {server, S}, {resource, R}} ->
            Client#client{jid = <<U/binary, "@", S/binary, "/", R/binary>>};
        _ ->
            Client
    end.

-spec send(escalus:client(), exml_stream:element()) -> ok.
send(#client{module = Mod, event_client = EventClient, rcv_pid = Pid, jid = Jid}, Elem) ->
    escalus_event:outgoing_stanza(EventClient, Elem),
    escalus_ct:log_stanza(Jid, out, Elem),
    Mod:send(Pid, Elem).

-spec get_stanza(client(), any()) -> exml_stream:element().
get_stanza(Conn, Name) ->
    get_stanza(Conn, Name, ?TIMEOUT).

-spec get_stanza(client(), any(), timeout()) -> exml_stream:element().
get_stanza(Client, Name, Timeout) ->
    case get_stanza_safe(Client, Timeout) of
        {error, timeout} ->
            throw({timeout, Name});
        {Stanza, _} ->
            Stanza
    end.

-spec get_stanza_with_metadata(client(), any(), timeout()) ->
    {exml_stream:element(), metadata()}.
get_stanza_with_metadata(Client, Name, Timeout) ->
    case get_stanza_safe(Client, Timeout) of
        {error, timeout} ->
            throw({timeout, Name});
        StanzaWithMetadata ->
            StanzaWithMetadata
    end.

-spec get_stanza_safe(client(), timeout()) ->
    {error, timeout} | {exml_stream:element(), map()}.
get_stanza_safe(#client{rcv_pid = Pid, jid = Jid}, Timeout) ->
    receive
        {stanza, Pid, Stanza, Metadata} ->
            escalus_ct:log_stanza(Jid, in, Stanza),
            {Stanza, Metadata}
    after Timeout ->
              {error, timeout}
    end.

get_stream_end(#client{rcv_pid = Pid, jid = Jid}, Timeout) ->
    receive
        {stanza, Pid, Stanza = #xmlel{name = <<"close">>}, _} ->
            escalus_ct:log_stanza(Jid, in, Stanza),
            Stanza;
        {stanza, Pid, Stanza = #xmlstreamend{}, _} ->
            escalus_ct:log_stanza(Jid, in, Stanza),
            Stanza
    after Timeout ->
              throw({timeout, stream_end})
    end.


-spec get_sm_h(client()) -> non_neg_integer().
get_sm_h(#client{module = escalus_tcp, rcv_pid = Pid}) ->
    escalus_tcp:get_sm_h(Pid);
get_sm_h(#client{module = Mod}) ->
    error({get_sm_h, {undefined_for_escalus_module, Mod}}).

-spec set_sm_h(client(), non_neg_integer()) -> {ok, non_neg_integer()}.
set_sm_h(#client{module = escalus_tcp, rcv_pid = Pid}, H) ->
    escalus_tcp:set_sm_h(Pid, H);
set_sm_h(#client{module = Mod}, _) ->
    error({set_sm_h, {undefined_for_escalus_module, Mod}}).

-spec set_filter_predicate(client(), filter_pred()) -> ok.
set_filter_predicate(#client{module = Module, rcv_pid = Pid}, Pred) ->
    Module:set_filter_predicate(Pid, Pred).

-spec reset_parser(client()) -> ok.
reset_parser(#client{module = Mod, rcv_pid = Pid}) ->
    Mod:reset_parser(Pid).

-spec is_connected(client()) -> boolean().
is_connected(#client{module = Mod, rcv_pid = Pid}) ->
    Mod:is_connected(Pid).

-spec stop(client()) -> ok | already_stopped.
stop(#client{module = Mod, rcv_pid = Pid} = Client) ->
    case is_connected(Client) of
        true ->
            end_stream(Client);
        _ ->
            ok
    end,
    Mod:stop(Pid).

%% @doc Brutally kill the connection without terminating the XMPP stream.
-spec kill(client()) -> ok | already_stopped.
kill(#client{module = Mod, rcv_pid = Pid}) ->
    Mod:kill(Pid).

-spec use_zlib(client()) -> ok.
use_zlib(#client{module = Mod, rcv_pid = Pid}) ->
    Mod:use_zlib(Pid).

-spec upgrade_to_tls(client()) -> ok.
upgrade_to_tls(#client{module = Mod, rcv_pid = Pid, props = Props}) ->
    SSLOpts = proplists:get_value(ssl_opts, Props, []),
    Mod:upgrade_to_tls(Pid, SSLOpts).

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
                             fun(([exml:element()], term(), integer()) -> term()), integer())
        -> term().
maybe_forward_to_owner(none, State, _Stanzas, _Fun, _Timestamp) ->
    State;
maybe_forward_to_owner(FilterPred, State, Stanzas, Fun, Timestamp)
    when is_function(FilterPred) ->
    AllowedStanzas = lists:filter(FilterPred, Stanzas),
    case AllowedStanzas of
        [] ->
            State;
        _ ->
            Fun(AllowedStanzas, State, Timestamp)
    end;
maybe_forward_to_owner(_, State, Stanzas, Fun, Timestamp) ->
    Fun(Stanzas, State, Timestamp).

-spec stanza_msg(Stanza :: exml:element(), Metadata :: map()) -> stanza_msg().
stanza_msg(Stanza, Metadata) ->
    {stanza, self(), Stanza, Metadata}.

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

-spec start_stream(client()) -> exml_stream:element().
start_stream(#client{module = Mod, props = Props} = Client) ->
    StreamStartReq = Mod:stream_start_req(Props),
    send(Client, StreamStartReq),
    Timeout = proplists:get_value(wait_for_stream_timeout, Props, 1000),
    StreamStartRep = get_stanza(Client, stream_start, Timeout),
    Mod:assert_stream_start(StreamStartRep, Props).

-spec end_stream(client()) -> exml_stream:element().
end_stream(#client{module = Mod, props = Props} = Client) ->
    StreamEndReq = Mod:stream_end_req(Props),
    send(Client, StreamEndReq),
    Timeout = proplists:get_value(wait_for_stream_end_timeout, Props, 5000),
    StreamEndRep = get_stream_end(Client, Timeout),
    Mod:assert_stream_end(StreamEndRep, Props).
