# Escalus as a standalone application

It't possible to use Escalus as a standalone application,
i.e. outside a Common Test test suite (and without any reliance
on the `common_test` application and its modules).
To do so some prerequisites must be met.

Firstly, the Escalus must be started just like any other application:

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
    otherwise the application won't start.

Note, that in a real security-conscious setting you probably shouldn't
store clear text user passwords in this file (though that's exactly what
the example does - remember Escalus is still mostly a testing tool).
