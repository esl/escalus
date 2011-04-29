%% Note: This module requires ejabberd_node and ejabberd_cookie
%%       to be set if common test configuration file

-module(escalus_ejabberd).

-export([rpc/3,
         remote_display/1,
         remote_format/1,
         remote_format/2]).

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
