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
         log_stanza/3,
         log_error/2]).

-define(APPNAME, escalus).

-include_lib("common_test/include/ct.hrl").

-spec add_log_link(any(), any(), any()) -> ok | false.
add_log_link(Heading, File, Type) ->
    is_ct_available() andalso
    case is_18_3_or_higher() of
        true -> ct_add_link(Heading, File, Type);
        false -> ct_logs:add_link(Heading, File, Type)
    end.

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
    case application:get_env(?APPNAME, config_file) of
        undefined ->
            error({escalus_error, no_config_file});
        {ok, ConfigFile} ->
            Path = interpret_config_file_path(ConfigFile),
            {ok, Config} = file:consult(Path),
            proplists:get_value(Option, Config)
    end.

interpret_config_file_path("/" ++ _ = AbsPath) ->
    AbsPath;
interpret_config_file_path(RelPath) ->
    case code:is_loaded(?MODULE) of
        {file, EscalusBeamPath} ->
            GetProjectDir = fun(Path) ->
                                    filename:dirname(filename:dirname(Path))
                            end,
            ProjectDir = GetProjectDir(EscalusBeamPath),
            filename:join([ProjectDir, RelPath]);
        _ ->
            error({escalus_error, beam_not_loaded})
    end.

-spec log_stanza(undefined | binary(), in | out, exml_stream:element()) -> ok.
log_stanza(undefined, _, _) -> ok;
log_stanza(Jid, Direction, Stanza) ->
    case get_stanza_log() of
        false ->
            ok;
        LogTarget ->
            do_log_stanza(LogTarget, Jid, Direction, Stanza)
    end.

get_stanza_log() ->
    case is_ct_available() of
        true ->
            case ct:get_config(stanza_log, console_and_file) of
                false ->
                    false;
                true ->
                    console_and_file;
                LogTarget when LogTarget == console_and_file;
                               LogTarget == console;
                               LogTarget == file  ->
                    LogTarget
            end;
        false ->
            false
    end.

-spec is_ct_available() -> boolean().
is_ct_available() ->
    code:is_loaded(ct) =/= false andalso ct:get_status() =/= no_tests_running.

-spec do_log_stanza(Target, Jid, Direction, Stanza) -> ok when
      Target :: console_and_file | console | file,
      Jid :: binary(),
      Direction :: in | out,
      Stanza :: exml:element().
do_log_stanza(Target, Jid, Direction, Stanza) ->
    ReportString = io_lib:format("~s ~p", [Jid, Direction]),
    PrettyStanza = try
                       iolist_to_binary(exml:to_pretty_iolist(Stanza))
                   catch error:Error ->
                             ct:pal(error, "Cannot convert stanza to iolist: ~s~n~p",
                                    [ReportString, Stanza]),
                             ct:fail(Error)
                   end,
    ct_print_or_log(Target, ReportString, PrettyStanza).

ct_print_or_log(Target, ReportString, PrettyStanza) ->
    CTFun = case Target of
                console_and_file -> pal;
                console -> print;
                file -> log
            end,
    %% grep anchor: ct:pal, ct:print, ct:log
    ct:CTFun(stanza_log, ?STD_IMPORTANCE, "~s~n~s", [ReportString, PrettyStanza], [esc_chars]).

%% ------------- Common Test hack! -------------
%% There is a bug in Common Test since 18.3, which causes links to be printed inside <pre/>.
%% This hack is a copy & paste from ct_logs.erl with a modification.
%% A patch will most probably be submitted to OTP team soon.

is_18_3_or_higher() ->
    lists:map(fun erlang:list_to_integer/1,
              string:tokens(erlang:system_info(version), ".")) >= [7, 3].

ct_add_link(Heading, File, Type) ->
    ct_log(Heading, "<a href=\"~ts\" type=~p>~ts</a>\n",
           [ct_uri(filename:join("log_private", File)), Type, File]).

ct_uri("") -> "";
ct_uri(Href) -> test_server_ctrl:uri_encode(Href).

ct_log(Heading, Format, Args) ->
    ct_logs ! {log, sync, self(), group_leader(), ct_internal, 99,
               [{hd, ct_int_header(), [ct_log_timestamp(os:timestamp()), Heading]},
                {Format, Args},
                {ft, ct_int_footer(), []}],
               false},
    true.

ct_int_header() ->
    "</pre><div class=\"ct_internal\"><b>*** CT ~s *** ~ts</b>".

ct_int_footer() ->
    "</div><pre>".

ct_log_timestamp({MS, S, US}) ->
    put(log_timestamp, {MS, S, US}),
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_local_time({MS, S, US}),
    MilliSec = trunc(US/1000),
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B "
                                "~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
                                [Year, Month, Day, Hour, Min, Sec, MilliSec])).

log_error(Format, Args) ->
    ct:pal(error, Format, Args).
