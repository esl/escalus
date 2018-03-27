-module(escalus_component).

-include_lib("escalus.hrl").
-include_lib("exml/include/exml_stream.hrl").

-behaviour(gen_server).

%% gen_server state
-record(component_state, {module,
                          features,
                          client,
                          user_state}).


%% escalus_component APIs
-export([start_link/3,
         set_filter/2,
         send/2]).

%% gen_server API
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).


%% escalus_component behaviour declaration
-callback init(term()) -> {ok, term()}.
-callback process_stanza(exml:element(), escalus_client:client(), term()) -> {ok, term()}.


start_link(Module, ConnectionArgs, Args) ->
    gen_server:start_link(?MODULE, {Module, ConnectionArgs, Args}, []).

set_filter(Component, FilterFN) ->
    gen_server:call(Component, {set_filter, FilterFN}).

send(Component, Stanza) ->
    gen_server:call(Component, {send, Stanza}).

init({Module, ConnectionArgs, Args}) ->
    {ok, Client, Features} = escalus_connection:start(ConnectionArgs,
                                                      [fun component_start_stream/2,
                                                       fun component_handshake/2]),
    {ok, UserState} = Module:init(Args),
    {ok, #component_state{module     = Module,
                          client     = Client,
                          features   = Features,
                          user_state = UserState}}.

handle_call({set_filter, FilterFN}, _From, #component_state{client = Client} = State) ->
    escalus_connection:set_filter_predicate(Client, FilterFN),
    {reply, ok, State};
handle_call({send, Stanza}, _From, #component_state{client = Client} = State) ->
    escalus_connection:send(Client, Stanza),
    {reply, ok, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info({stanza, Pid, Stanza}, #component_state{module     = M,
                                                    user_state = S,
                                                    client     = #client{rcv_pid = Pid}} = State) ->
    {ok, NewS} = M:process_stanza(Stanza, State#component_state.client, S),
    {noreply, State#component_state{user_state = NewS}};
handle_info(_Info, State) -> {noreply, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% code below is taken from MIM's component_SUITE module
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