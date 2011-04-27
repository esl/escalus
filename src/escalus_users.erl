-module(escalus_users).

% Public API
-export([get_jid/1,
         get_username/1,
         get_users/1,
         get_userspec/2,
         get_user_by_name/1,
         create_users/1,
         delete_users/1,
         get_usp/1,
         make_everyone_friends/1]).

-include_lib("exmpp/include/exmpp_client.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

get_jid(Name) ->
    {Name, Spec} = get_user_by_name(Name),
    [U, S, _] = get_usp(Spec),
    U ++ "@" ++ S.

get_username(Name) ->
    {Name, Spec} = get_user_by_name(Name),
    [U, _, _] = get_usp(Spec),
    U.

get_users(all) ->
    ct:get_config(escalus_users);
get_users({by_name, Names}) ->
    All = get_users(all),
    [get_user_by_name(Name, All) || Name <- Names];
get_users(Users) ->
    Users.

get_user_by_name(Name) ->
    get_user_by_name(Name, get_users(all)).

get_userspec(Config, Username) ->
    {escalus_users, Users} = proplists:lookup(escalus_users, Config),
    {Username, UserSpec} = proplists:lookup(Username, Users),
    UserSpec.

create_users(Users) ->
    lists:map(fun create_user/1, get_users(Users)).

delete_users(Users) ->
    lists:foreach(fun delete_user/1, get_users(Users)).

get_usp(UserSpec) ->
    {username, Username} = proplists:lookup(username, UserSpec),
    {server, Server} = proplists:lookup(server, UserSpec),
    {password, Password} = proplists:lookup(password, UserSpec),
    [Username, Server, Password].

make_everyone_friends(Users) ->
    send_to_everyone(Users, exmpp_presence:subscribe()),
    send_to_everyone(Users, exmpp_presence:subscribed()).

send_to_everyone(Users, BaseStanza) ->
    Config = escalus_cleaner:start([]),
    NamesNJids = [{Name, get_jid(Name)} || {Name, _Spec} <- Users],
    lists:foreach(fun ({Name, UserSpec}) ->
        Client = escalus_client:start_wait(Config, UserSpec, "friendly"),
        lists:foreach(
            fun ({OtherName, Jid}) when OtherName =/= Name ->
                    Stanza = exmpp_stanza:set_recipient(BaseStanza, Jid),
                    escalus_client:send_wait(Client, Stanza);
                (_) -> ok
            end, NamesNJids)
    end, Users),
    escalus_cleaner:stop(Config).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_user_by_name(Name, Users) ->
    {Name, _} = proplists:lookup(Name, Users).

create_user({_Name, UserSpec} = FullSpec) ->
    Session = escalus_client:start_session([], UserSpec, random),
    GetFields = exmpp_client_register:get_registration_fields(),
    exmpp_session:send_packet(Session, GetFields),
    {ok, RegisterInstrs} = wait_for_result("create user"),
    Id = exmpp_stanza:get_id(GetFields),
    FieldKeys = get_registration_questions(RegisterInstrs),
    Fields = [proplists:lookup(Key, UserSpec) || Key <- FieldKeys],
    Register = exmpp_client_register:register_account(Id, Fields),
    exmpp_session:send_packet(Session, Register),
    wait_for_result("create user"),
    exmpp_session:stop(Session),
    FullSpec.

delete_user({_Name, UserSpec}) ->
    Session = escalus_client:start_session([], UserSpec, random),
    {ok, _JID} = escalus_client:login([], Session, UserSpec),
    Packet = exmpp_client_register:remove_account(),
    exmpp_session:send_packet(Session, Packet),
    wait_for_result("delete user"),
    exmpp_session:stop(Session).

wait_for_result(Action) ->
    receive
        #received_packet{packet_type=iq, type_attr="result", raw_packet=Raw} ->
            % RawStr = exmpp_xml:document_to_iolist(Raw),
            % error_logger:info_msg("success when trying to ~s: ~s~n", [Action, RawStr]),
            {ok, Raw};
        #received_packet{packet_type=iq, type_attr="error", raw_packet=Raw} ->
            RawStr = exmpp_xml:document_to_iolist(Raw),
            case is_conflict_stanza(Raw) of
                true ->
                    error_logger:info_msg("~s - skipping conflict stanza: ~s~n", [Action, RawStr]),
                    conflict;
                false ->
                    error_logger:error_msg("error when trying to ~s: ~s~n", [Action, RawStr]),
                    exit(failed_to_register)
            end
        after 1000 ->
            error_logger:error_msg("TIMEOUT~n", []),
            exit(timeout)
    end.

is_conflict_stanza(Stanza) ->
    exmpp_xml:get_attribute(Stanza, <<"type">>, x) == <<"error">> andalso
    exmpp_xml:get_path(Stanza, [{element, "error"}, {attribute, <<"code">>}]) == <<"409">>.

get_registration_questions(Stanza) ->
    Query = exmpp_xml:get_element(Stanza, "query"),
    Children = exmpp_xml:get_child_elements(Query),
    ChildrenNames = lists:map(fun exmpp_xml:get_name_as_atom/1, Children),
    ChildrenNames -- [instructions].
