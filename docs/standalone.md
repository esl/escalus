# Escalus as a standalone application

It's possible to use Escalus as a standalone application,
i.e. outside a Common Test test suite (and without any reliance
on the `common_test` application and its modules).
To do so some prerequisites must be met.

Firstly, Escalus must be started just like any other application:

    > application:ensure_all_started(escalus).

This makes predefined environment variables from `escalus.app` available
for access by `application:get_env`.
These options and their respective values for running without Common Test are:

    {env, [%% Set config_file in case of using Escalus without Common Test
           %% (i.e. common_test is false).
           {common_test, false},
           {config_file, "priv/escalus.config"}]}

To recap:

-   `common_test` must be false - this will tell Escalus not to rely on
    modules available in the `common_test` application;
    this also disallows the use of `escalus_ejabberd:rpc` and similar
    functions,

-   `config_file` **must** be set to a configuration file location;
    this location may be absolute or relative (in which case the file will
    be searched for relative to the project directory).

Note, that in a real security-conscious setting you probably shouldn't
store clear text user passwords in this file (though that's exactly what
the example does - remember Escalus is still mostly a testing tool).

If you don't want to rely on the application resource file
(`escalus.app`/`escalus.app.src`) you can set both of these options just
after loading Escalus:

    > application:ensure_all_started(escalus).
    > application:set_env(escalus, common_test, false).
    > application:set_env(escalus, config_file, "/absolute/or/relative/path").

Keep in mind that calling `application:ensure_all_started(escalus)` will
overwrite the values with stuff from `escalus.app`.
Set the variables after the application is started.

## Config file location

If the `config_file` value starts with `/` it's  interpreted as an
absolute path and left as is.
Otherwise, it's interpreted as a relative path to the project directory.
The project directory is the directory one level higher than the directory
containing `ejabberd_ct.beam`.
In case of a standard Git checkout the project directory is simply `escalus`.

    escalus/
    ├── .git/
    ├── ...
    ├── docs/
    ├── ebin/
    │   ├── ...
    │   ├── escalus_ct.beam
    │   └── ...
    ├── src/
    └── ...

## Example shell session

Fire an Erlang shell:

    erl -pa ebin deps/*/ebin

### Basic example

Run example:

    application:ensure_all_started(escalus).
    application:set_env(escalus, common_test, false).
    {ok, Config} = file:consult("priv/escalus.config").
    CarolSpec = escalus_users:get_options(Config, carol).
    {ok, Carol, _, _} = escalus_connection:start(CarolSpec).
    escalus_connection:send(Carol, escalus_stanza:chat_to(alice, "hi")).
    escalus_connection:stop(Carol).

### Story example

Please note that `escalus:story/3` and `escalus:create_users/2` are intended to be used in a testing environment, i.e. with Common Test available. Specifically, `escalus:create_users/2` will not work without Common Test and with non-XMPP registration method chosen (i.e. RPC based user registration). In case of MongooseIM or ejabberd, please ensure `mod_register` is enabled (and, depending on your scenario, probably configured not to send a _welcome message_).

Run example:

    X2SFun = fun(X) -> lists:flatten(io_lib:format("~p~n", [X])) end.
    {ok, Config0} = file:consult("priv/escalus.config").
    application:ensure_all_started(escalus).
    application:set_env(escalus, common_test, false).
    escalus:create_users(Config0, {by_name, [alice, mike]}).
    Config = escalus_event:start(escalus_cleaner:start(Config0)).
    SendFun = fun(A, B) -> escalus:send(A, escalus_stanza:chat_to(B, "hi")), ok end.
    RecvFun = fun(B) -> [S] = escalus:wait_for_stanzas(B, 1), {ok, S} end.
    StoryFun = fun(A, B) -> SendFun(A, B), {ok, S} = RecvFun(B), erlang:display(X2SFun(S)) end.
    escalus:story(Config, [{mike, 1}, {alice,1}], StoryFun).
    escalus_cleaner:stop(escalus_event:stop(Config)).
    escalus:delete_users(Config, {by_name, [alice, mike]}).
