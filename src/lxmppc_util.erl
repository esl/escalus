%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Various utilities
%%% @end
%%%===================================================================
-module(lxmppc_util).

-export([get_stanza/2]).

-include_lib("exml/include/exml.hrl").
-include("lxmppc.hrl").

-define(TIMEOUT, 1000).

-spec get_stanza(#transport{}, any()) -> #xmlelement{}.
get_stanza(Conn, Name) ->
    receive
        {stanza, Conn, Stanza} ->
            Stanza
    after ?TIMEOUT ->
            throw({timeout, Name})
    end.
