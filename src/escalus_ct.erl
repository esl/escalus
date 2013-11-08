-module(escalus_ct).

%% @doc This module abstracts away the calls to Common Test.
%%
%% The module doesn't try to simulate all of them as, in general,
%% the user should be aware whether the code he/she writes is run
%% within a Common Test suite or not.
%%
%% Where applicable functions return false if Common Test is not available
%% and the value of the inner call to Common Test otherwise.
%% @end

-export([add_log_link/3,
         fail/1,
         get_config/1,
         is_ct_available/0]).

%% What about that?
-export([rpc_call/6]).

-spec add_log_link(any(), any(), any()) -> ok | false.
add_log_link(Heading, File, Type) ->
    is_ct_available() andalso ct_logs:add_link(Heading, File, Type).

-spec fail(any()) -> no_return().
fail(Reason) ->
    case is_ct_available() of
        true -> ct:fail(Reason);
        false -> error({escalus_error, Reason})
    end.

-spec get_config(any()) -> any() | undefined | no_return().
get_config(Required) ->
    case is_ct_available() of
        true -> ct:get_config(Required);
        false -> consult_config_file(Required)
    end.

%% If performance becomes an issue the result of file:consult/1
%% might be cached and lists:keyfind/3 used in place of proplists:get_value/2
%% (watch out - these return different values on lookup failure).
consult_config_file(Option) ->
    case application:get_env(config_file) of
        undefined ->
            error({escalus_error, no_config_file});
        {ok, ConfigFile} ->
            {ok, Config} = file:consult(ConfigFile),
            proplists:get_value(Option, Config)
    end.

rpc_call(Node, Module, Function, Args, TimeOut, Cookie) ->
    case is_ct_available() of
        true ->
            ct_rpc:call(Node, Module, Function, Args, TimeOut, Cookie);
        false ->
            %% TODO: don't error out, should be easy to simulate ct_rpc:call/6
            error({escalus_error, common_test_unavailable})
    end.

-spec is_ct_available() -> boolean().
is_ct_available() ->
    case application:get_env(common_test) of
        {ok, true} ->
            true;
        _ ->
            false
    end.
