-module(escalus_component).

-include_lib("escalus.hrl").
-include_lib("exml/include/exml_stream.hrl").

-behaviour(gen_server).

%% gen_server state
-record(component_state, {module :: module(),
                          features :: escalus_session:features(),
                          client :: escalus_client:client(),
                          user_state :: any()}).

-type state() :: #component_state{}.
-type server_name() :: {local, atom()} | {global, any()} | {via, module(), any()}.


%% escalus_component APIs
-export([start_link/3,
         start_link/4,
         start/3,
         start/4,
         stop/2,
         set_filter/2,
         send/2]).

%% gen_server behaviour API
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% timeout definitions
-define(WAIT_AFTER_STANZA, 0).
-define(WAIT_AFTER_TIMEOUT, 100).


%% escalus_component behaviour declaration
-callback init(InitParameters :: term()) -> {ok, InitialState :: term()}.
-callback process_stanza(Stanza :: exml:element(),
                         XMPPClient :: escalus_client:client(),
                         State :: term()) -> {ok, NewState :: term()}.
-callback handle_info(Info :: term(),
                      XMPPClient :: escalus_client:client(),
                      State :: term()) -> {ok, NewState :: term()}.
-callback terminate(Reason :: term(), State :: term()) -> any().
-otpional_callbacks([terminate/2, handle_info/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% escalus_component APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(module(), any(), any()) -> {'ok', pid()}.
start_link(Module, ConnectionArgs, Args) ->
    gen_server:start_link(?MODULE, {Module, ConnectionArgs, Args}, []).

-spec start_link(server_name(), module(), any(), any()) -> {'ok', pid()}.
start_link(ServerName, Module, ConnectionArgs, Args) ->
    gen_server:start_link(ServerName, ?MODULE, {Module, ConnectionArgs, Args}, []).

-spec start(module(), any(), any()) -> {'ok', pid()}.
start(Module, ConnectionArgs, Args) ->
    gen_server:start(?MODULE, {Module, ConnectionArgs, Args}, []).

-spec start(server_name(), module(), any(), any()) -> {'ok', pid()}.
start(ServerName, Module, ConnectionArgs, Args) ->
    gen_server:start(ServerName, ?MODULE, {Module, ConnectionArgs, Args}, []).

-spec stop(pid(), any()) -> 'ok'.
stop(Component, Reason) ->
    gen_server:cast(Component, {stop, Reason}).

-spec set_filter(pid(), function()) -> 'ok'.
set_filter(Component, FilterFN) ->
    gen_server:call(Component, {set_filter, FilterFN}).

-spec send(pid(), exml:element()) -> 'ok'.
send(Component, Stanza) ->
    gen_server:call(Component, {send, Stanza}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server behaviour
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(any()) -> {'ok', state(), timeout()}.
init({Module, ConnectionArgs, Args}) ->
    {ok, Client, Features} = escalus_connection:start(ConnectionArgs,
                                                      [fun component_start_stream/2,
                                                       fun component_handshake/2]),
    escalus_tcp:set_active(Client#client.rcv_pid, once),
    process_flag(trap_exit,true),
    {ok, UserState} = Module:init(Args),
    State = #component_state{module     = Module,
                             client     = Client,
                             features   = Features,
                             user_state = UserState},
    {ok, State, ?WAIT_AFTER_TIMEOUT}.


-spec handle_call(any(), {pid(), any()}, state()) -> {'reply', any(), state(), timeout()}.
handle_call({set_filter, FilterFN}, _From, #component_state{client = Client} = State) ->
    escalus_connection:set_filter_predicate(Client, FilterFN),
    {reply, ok, State, ?WAIT_AFTER_STANZA};
handle_call({send, Stanza}, _From, #component_state{client = Client} = State) ->
    escalus_connection:send(Client, Stanza),
    {reply, ok, State, ?WAIT_AFTER_STANZA};
handle_call(_Request, _From, State) ->
    {reply, ok, State, ?WAIT_AFTER_STANZA}.


-spec handle_cast(any(), state()) -> {'noreply', state(), timeout()}.
handle_cast({stop, Reason}, State) ->
    {stop, Reason, State};
handle_cast(_Request, State) ->
    {noreply, State, ?WAIT_AFTER_STANZA}.


-spec handle_info(any(), state()) -> {'noreply', state(), timeout()}.
handle_info({stanza, Pid, Stanza, _}, #component_state{client     = #client{rcv_pid = Pid},
                                                    module     = M,
                                                    user_state = S} = State) ->
    {ok, NewS} = M:process_stanza(Stanza, State#component_state.client, S),
    {noreply, State#component_state{user_state = NewS}, ?WAIT_AFTER_STANZA};
handle_info(timeout, #component_state{client = #client{rcv_pid = Pid}} = State) ->
    escalus_tcp:set_active(Pid, once),
    {noreply, State, ?WAIT_AFTER_TIMEOUT};
handle_info({'EXIT', _From, Reason}, State) ->
    {stop, Reason, State};
handle_info(Info, #component_state{module = M, client = C, user_state = S} = State) ->
    NewState = case erlang:function_exported(M, handle_info, 3) of
                   false -> State;
                   true ->
                       {ok, NewS} = M:handle_info(Info, C, S),
                       State#component_state{user_state = NewS}
               end,
    {noreply, NewState, ?WAIT_AFTER_STANZA}.


terminate(Reason, #component_state{client = C, module = M, user_state = S}) ->
    catch escalus_connection:stop(C),
    case erlang:function_exported(M, terminate, 2) of
        false -> ok;
        true -> M:terminate(Reason, S)
    end.

-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions, code below is taken from MIM's component_SUITE module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%--------------------------------------------------------------------
%% Escalus connection steps
%%--------------------------------------------------------------------
component_start_stream(Conn = #client{props = Props}, []) ->
    {component, Component} = lists:keyfind(component, 1, Props),

    StreamStart = component_stream_start(Component),
    ok = escalus_connection:send(Conn, StreamStart),
    StreamStartRep = escalus_connection:get_stanza(Conn, wait_for_stream),

    #xmlstreamstart{attrs = Attrs} = StreamStartRep,
    Id = proplists:get_value(<<"id">>, Attrs),

    {Conn#client{props = [{sid, Id} | Props]}, []}.


component_handshake(Conn = #client{props = Props}, []) ->
    {password, Password} = lists:keyfind(password, 1, Props),
    {sid, SID} = lists:keyfind(sid, 1, Props),

    Handshake = component_handshake_el(SID, Password),
    ok = escalus_connection:send(Conn, Handshake),

    HandshakeRep = escalus_connection:get_stanza(Conn, handshake),
    case HandshakeRep of
        #xmlel{name = <<"handshake">>, children = []} ->
            {Conn, []};
        #xmlel{name = <<"stream:error">>} ->
            throw({stream_error, HandshakeRep})
    end.


%%--------------------------------------------------------------------
%% Stanzas
%%--------------------------------------------------------------------
component_stream_start(Component) ->
    Attrs = [{<<"to">>, Component},
             {<<"xmlns">>, <<"jabber:component:accept">>},
             {<<"xmlns:stream">>,
              <<"http://etherx.jabber.org/streams">>}],
    #xmlstreamstart{name = <<"stream:stream">>, attrs = Attrs}.

component_handshake_el(SID, Password) ->
    Handshake = crypto:hash(sha, <<SID/binary, Password/binary>>),
    #xmlel{name     = <<"handshake">>,
           children = [#xmlcdata{content = base16:encode(Handshake)}]}.