-module(escalus_history_h).

-behaviour(gen_event).

-export([get_history/1]).

-export([init/1,
         terminate/2,
         handle_info/2,
         handle_call/2,
         handle_event/2]).

-record(state, {
        events :: list()
}).

-type state() :: #state{}.

-spec get_history(escalus_event:manager()) -> list().
get_history(Mgr) ->
    gen_event:call(Mgr, escalus_history_h, get_history).

-spec init([]) -> {ok, state()}.
init([]) ->
    S = #state{
            events = []
        },
    {ok, S}.

-spec handle_event(term(), state()) -> {ok, state()}.
handle_event({incoming_stanza, Jid, Stanza}, State) ->
    {ok, save_stanza(incoming_stanza, Jid, Stanza, State)};
handle_event({outgoing_stanza, Jid, Stanza}, State) ->
    {ok, save_stanza(outgoing_stanza, Jid, Stanza, State)};
handle_event({pop_incoming_stanza, Jid, Stanza}, State) ->
    {ok, save_stanza(pop_incoming_stanza, Jid, Stanza, State)};
handle_event(story_start, State) ->
    {ok, save_story_event(story_start, State)};
handle_event(story_end, State) ->
    {ok, save_story_event(story_end, State)};
handle_event(_Event, State) ->
    {ok, State}.

-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(_, State) ->
    {ok, State}.

-spec handle_call(get_history, state()) -> {ok, list(), state()}.
handle_call(get_history, State=#state{events=Events}) ->
    {ok, lists:reverse(Events), State}.

-spec terminate(term(), state()) -> ok.
terminate(_, _) ->
    ok.

%% ===================================================================
%% Helpers
%% ===================================================================

save_stanza(Type, Jid, Stanza, State=#state{events = Events}) ->
    State#state{
        events = [{stanza, Type, Jid, erlang:system_time(microsecond), Stanza}|Events]}.

save_story_event(Type, State=#state{events = Events}) ->
    State#state{
        events = [{story, Type, erlang:system_time(microsecond)}|Events]}.
