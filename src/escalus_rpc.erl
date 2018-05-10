%% @doc This module should not exist as it has nothing to do with XMPP
%% nor is it needed by Escalus itself.
%%
%% The module contains RPC helpers which used to rely on Common Test,
%% but do not do so anymore.
-module(escalus_rpc).

%% Public
-export([call/6]).

%%
%% Public
%%

%% @doc Emulate `ct_rpc:call/6' but do not rely on Common Test.
%% `call/6' takes a `Cookie' as the last parameter,
%% so that nodes with different cookies can be easily called.
%% %% However, this function is not safe (and neither is the original `ct_rpc:call/6')
%% in a concurrent environment as it gets/sets the cookie
%% with `erlang:get_cookie/0' and `erlang:set_cookie/1'.
%% Interleaving these calls in concurrent processes is prone to race conditions.
call(Node, Module, Function, Args, TimeOut, Cookie) ->
    call_with_cookie_match(Node, Module, Function, Args, TimeOut, Cookie).

%%
%% Internal
%%

%% Copied from ct_rpc and renamed.
call_with_cookie_match(Node, Module, Function, Args, TimeOut, Cookie) when is_atom(Node) ->
    Cookie0 = set_the_cookie(Cookie),
    Result = case rpc:call(Node, Module, Function, Args, TimeOut) of
                 {badrpc, Reason} ->
                     error({badrpc, Reason}, [Node, Module, Function, Args, TimeOut, Cookie]);
                 R ->
                     R
             end,
    _ = set_the_cookie(Cookie0),
    Result.

%% Copied from ct_rpc.
set_the_cookie([]) ->
    [];
set_the_cookie(Cookie) ->
    Cookie0 = erlang:get_cookie(),
    erlang:set_cookie(node(),Cookie),
    Cookie0.
