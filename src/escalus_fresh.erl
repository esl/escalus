-module(escalus_fresh).
-export([story/3, story_with_config/3]).

% TODO unify this type across escalus
-type userspec() :: {atom(), integer()}.
-type config() :: escalus:config().

-spec story_with_config(config(), [userspec()], fun()) -> any().
story_with_config(Config, UserSpecs, StoryFun) ->
    {FreshConfig, FreshSpecs} = create_fresh_users(Config, UserSpecs),
    escalus:story(FreshConfig, UserSpecs,
                  fun(Args) -> apply(StoryFun, [FreshConfig|Args]) end).

story(Config, UserSpecs, StoryFun) ->
    {FreshConfig, FreshSpecs} = create_fresh_users(Config, UserSpecs),
    escalus:story(FreshConfig, UserSpecs, StoryFun).

create_fresh_users(Config, UserSpecs) ->
    FreshSpecs = fresh_specs(Config, UserSpecs, fresh_suffix()),
    case length(FreshSpecs) == length(UserSpecs) of
        false -> ct:fail("failed to get required users"); _ -> ok end,
    FreshConfig = escalus_users:create_users(Config, FreshSpecs),
    {FreshConfig, FreshSpecs}.

fresh_specs(Config, TestedUsers, StorySuffix) ->
    AllSpecs = escalus_config:get_config(escalus_users, Config),
    [ make_fresh_username(Spec, StorySuffix)
      || Spec <-select(TestedUsers, AllSpecs) ].

make_fresh_username({N, UserConfig}, Suffix) ->
    {username, OldName} = proplists:lookup(username, UserConfig),
    NewName = << OldName/binary, Suffix/binary >>,
    {N, lists:keyreplace(username, 1, UserConfig, {username, NewName})}.

select(UserResources, FullSpecs) ->
    Fst = fun({A, _}) -> A end,
    UserNames = lists:map(Fst, UserResources),
    lists:filter(fun({Name, _}) -> lists:member(Name, UserNames) end,
                 FullSpecs).

fresh_suffix() ->
    {_, S, US} = erlang:now(),
    L = lists:flatten([integer_to_list(S rem 100), ".", integer_to_list(US)]),
    list_to_binary(L).
