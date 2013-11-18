# Escalus as a standalone application

It't possible to use Escalus as a standalone application,
i.e. outside a Common Test test suite (and without any reliance
on the `common_test` application and its modules).
To do so some prerequisites must be met.

Firstly, Escalus must be started just like any other application:

    > application:start(escalus).

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

    > application:start(escalus).
    > application:set_env(escalus, common_test, false).
    > application:set_env(escalus, config_file, "/absolute/or/relative/path").

Keep in mind setting them before calling `application:start(escalus)`
will overwrite the values with stuff from `escalus.app`.

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
