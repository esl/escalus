-module(escalus_fresh_SUITE).
-compile(export_all).

all() ->
    [ it_creates_a_global_table_on_startup,
      it_registers_fresh_user_specs_in_table,
      previously_registered_users_can_be_removed_in_one_fell_swoop,
      fresh_users_can_be_created_outside_a_story
    ].

it_creates_a_global_table_on_startup(_) ->
    given_escalus_started(empty_config()),
    %% then
    [] = ets:tab2list(internal_fresh_db()).

it_registers_fresh_user_specs_in_table(_) ->
    C = [{escalus_users,
          [{al, [{username, <<"a">>}, {server, <<"0.0.0.0">>}, {password, <<"p">>}]}]}]
        ++ empty_config(),
    given_escalus_started(C),

    % when
    catch(escalus_fresh:story(C, [{al, 1}], fun id/1)),

    % then
    [{Suffix, [{escalus_users, [{al, [{username, <<"a", Suffix/binary>>}, _, _]}]},_]}]
        = ets:tab2list(internal_fresh_db()).

previously_registered_users_can_be_removed_in_one_fell_swoop(_) ->
    C = [{escalus_users,
          [{al, [{username, <<"a">>}, {server, <<"0.0.0.0">>}, {password, <<"p">>}]},
           {bo, [{username, <<"b">>}, {server, <<"0.0.0.0">>}, {password, <<"p">>}]},
           {cat, [{username, <<"c">>}, {server, <<"0.0.0.0">>}, {password, <<"p">>}]}
          ]}] ++ empty_config(),
    given_escalus_started(C),
    catch(escalus_fresh:story(C, [{al, 1}, {bo, 1}, {cat, 1}], fun id/1)),

    % when
    escalus_fresh:clean(),
    % then
    [] = ets:tab2list(internal_fresh_db()).

fresh_users_can_be_created_outside_a_story(_) ->
    C = [{escalus_users,
          [{ada, [{username, <<"ada">>},
                  {server, <<"0.0.0.0">>},
                  {password, <<"p">>}]}
          ]}] ++ empty_config(),
    given_escalus_started(C),

    % when
    escalus_fresh:create_users(C, [{ada, 1}]),

    % then
    [{Suffix,
      [{escalus_users,[{ada,[{username,<<"ada" , Suffix/binary>>},
                             {server,<<"0.0.0.0">>},
                             {password,<<"p">>}]}]},
       _
      ]}]
     = ets:tab2list(internal_fresh_db()).


id(A) -> A.
empty_config() -> [{escalus_user_db, {module, ?MODULE, []}}].
given_escalus_started(C) ->
    catch(escalus:end_per_suite(C)),
    %% init_per happens in a different process that the stories
    S = self(),
    spawn(fun() -> escalus:init_per_suite(C), S ! sweet end),
    receive sweet -> ok after 5000 -> error(failed_to_init_escalus) end.

internal_fresh_db() -> escalus_fresh_db.

%% escalus_user_db API
%% These can be used for verifying the internal calls, if desired
start(C) -> C.
stop(C) -> C.
create_users(C, Specs) -> lists:keystore(escalus_users, 1, C, {escalus_users, Specs}).
delete_users(_C, _) -> {ok, deleted}.
