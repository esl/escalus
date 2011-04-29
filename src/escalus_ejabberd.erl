%% Note: This module requires ejabberd_node and ejabberd_cookie
%%       to be set if common test configuration file

-module(escalus_ejabberd).

-export([rpc/3,
         remote_display/1,
         remote_format/1,
         remote_format/2,
         create_users/1,
         create_users/2,
         delete_users/1]).

rpc(M, F, A) ->
    Node = ct:get_config(ejabberd_node),
    Cookie = ct:get_config(ejabberd_cookie),
    ct_rpc:call(Node, M, F, A, 3000, Cookie).


remote_display(String) ->
    Line = [$\n, [$- || _ <- String], $\n],
    remote_format("~s~s~s", [Line, String, Line]).

remote_format(Format) ->
    remote_format(Format, []).

remote_format(Format, Args) ->
    group_leader(rpc(erlang, whereis, [user]), self()),
    io:format(Format, Args),
    group_leader(whereis(user), self()).

% API - compatible with escalus_users:create_users/1
create_users(Config) ->
    create_users(Config, all).

% API - compatible with escalus_users:create_users/2
create_users(Config, Who) ->
    AllUsers = escalus_users:get_users(Who),
    lists:foreach(fun({_Name, UserSpec}) ->
        register_user(UserSpec)
    end, AllUsers),
    [{escalus_users, AllUsers} | Config].

% API - compatible with escalus_users:delete_users/1
delete_users(Config) ->
    {escalus_users, AllUsers} = proplists:lookup(escalus_users, Config),
    lists:foreach(fun({_Name, UserSpec}) ->
        unregister_user(UserSpec)
    end, AllUsers).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

register_user(UserSpec) ->
    rpc(ejabberd_admin, register, escalus_users:get_usp(UserSpec)).

unregister_user(UserSpec) ->
    [U, S, _P] = escalus_users:get_usp(UserSpec),
    rpc(ejabberd_admin, unregister, [U, S]).
