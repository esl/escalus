%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Module for manipulating JIDs
%%% @end
%%%===================================================================

-module(lxmppc_jid).

-include("lxmppc.hrl").

-export([bare/1]).
%% -export([parse/1, full/1]).

bare(#jid{user=User, server=Server}) ->
    <<User/binary, "@", Server/binary>>.
