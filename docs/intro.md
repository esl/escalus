The [test/example_SUITE.erl][example_SUITE] file contains a minimalistic
example of an Escalus test suite.

[example_SUITE]: /test/example_SUITE.erl

You should include `escalus.hrl` file and the Common Test header:

    -include_lib("escalus/include/escalus.hrl").
    -include_lib("common_test/include/ct.hrl").

Escalus contains functions `escalus:init_per_suite/1`,
`escalus:end_per_suite/1`, `escalus:init_per_testcase` and
`escalus:end_per_testcase` which should be called in
appropriate Common Test callback functions.

You can specify users that will take part in your tests
in Common Test config files,
look at [test/test.config][test_config] file that comes with Escalus:

    {escalus_users, [
        {alice, [
            {username, "alice"},
            {server, "localhost"},
            {password, "makota"}]},
        {bob, [
            {username, "bob"},
            {server, "localhost"},
            {password, "bobcat"}]}
    ]}.

[test_config]: /test/test.config

Escalus can create and delete those users if
server under test supports in-band registration
[XEP-0077](http://xmpp.org/extensions/xep-0077.html)
and has no limits on number of registrations
per second (configure `registration_timeout` to `infinity`
in case of ejabberd).

You create and delete the users by calling `escalus:create_users/1`
and `escalus:delete_users/1`:

    init_per_group(_GroupName, Config) ->
        escalus:create_users(Config).

    end_per_group(_GroupName, Config) ->
        escalus:delete_users(Config).

In our exemplary test case it is done in `init_per_group` and `end_per_group`
functions, but you could as well do it in `init-/end_per_suite` if you prefer.
Deleting users should clean all their data (e.g. roster buddies), so it
improves test isolation, but takes longer.

In most of the test cases you will use `escalus:story/3` function.
Story wraps all the test and does the cleanup and initialisation:

    messages_story(Config) ->
        escalus:story(Config, [1, 1], fun(Alice, Bob) ->

            %% Alice sends a message to Bob
            escalus:send(Alice, escalus_stanza:chat_to(Bob, "OH, HAI!")),

            %% Bob gets the message
            escalus:assert(is_chat_message, ["OH, HAI!"],
                           escalus:wait_for_stanza(Bob))

        end).

The story above involves two users (second argument is a two-element list)
each having one resource (list contains ones). As you see from
the config files, those users are Alice and Bob. Escalus logs in
users at the beginning of the story and logs them out after it ends
(either successfully or by crash).

It's also possible to designate users taking part in a story more
specifically:

    messages_story(Config) ->
        escalus:story(Config, [{alice, 1}, {kate, 1}], fun(Alice, Kate) ->
            ...
        end).

That allows one to choose users which are not consecutive
in [test/test.config][test_config].

Inside the story you can use `escalus:send/2` function to send
stanzas, functions from `escalus_stanza` module to create them
and `escalus:wait_for_stanza` to receive them.

`wait_for_stanza` makes test fail if no stanza arrives
up to a timeout. There is also `wait_for_stanzas` function which
takes number of stanzas N as an argument and returns N-element or
shorter list of stanzas, returning less stanzas instead of crashing.
Both `wait_for_stanza` and `wait_for_stanzas` can take an extra argument --
timeout in milliseconds. The default timeout value is one second.

You make assertions using `escalus:assert/3` function.
First argument is the predicate. It can be a fun,
a `{module, function}` tuple or an atom. Atoms refer
to functions from `escalus_pred` module. Second
argument is a parameter list and third is a stanza that
we assert things about. There is `escalus:assert/2`
function that is equivalent to `assert/3` with empty
parameter list. Calling `escalus:assert(Pred, [Param1, Param2], Stanza)`
makes sure that Pred(Param1, Param2, Stanza) yields true.
Stanza is separate from parameters to improve error reporting.
