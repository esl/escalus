-module(escalus_users).

% Public API
-export([get_users/1,
         create_users/1,
         delete_users/1,
         get_usp/1]).

-include_lib("exmpp/include/exmpp_client.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

get_users(all) ->
    ct:get_config(escalus_users);
get_users({by_name, Names}) ->
    All = get_users(all),
    [{Name, _} = proplists:lookup(Name, All) || Name <- Names];
get_users(Users) ->
    Users.

create_users(Users) ->
    lists:map(fun create_user/1, get_users(Users)).

delete_users(Users) ->
    lists:foreach(fun delete_user/1, get_users(Users)).

get_usp(UserSpec) ->
    {username, Username} = proplists:lookup(username, UserSpec),
    {server, Server} = proplists:lookup(server, UserSpec),
    {password, Password} = proplists:lookup(password, UserSpec),
    [Username, Server, Password].

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

create_user({_Name, UserSpec} = FullSpec) ->
    Session = escalus_client:start_session(UserSpec),
    GetFields = exmpp_client_register:get_registration_fields(),
    exmpp_session:send_packet(Session, GetFields),
    wait_for_result("create user"),
    Id = exmpp_stanza:get_id(GetFields),
    Fields = [proplists:lookup(Key, UserSpec) || Key <- [username, password]],
    Register = exmpp_client_register:register_account(Id, Fields),
    exmpp_session:send_packet(Session, Register),
    wait_for_result("create user"),
    exmpp_session:stop(Session),
    FullSpec.

delete_user({_Name, UserSpec}) ->
    Session = escalus_client:start_session(UserSpec),
    {ok, _JID} = escalus_client:login(Session, UserSpec),
    Packet = exmpp_client_register:remove_account(),
    exmpp_session:send_packet(Session, Packet),
    wait_for_result("delete user"),
    exmpp_session:stop(Session).

wait_for_result(Action) ->
    receive
        #received_packet{packet_type=iq, type_attr="result", raw_packet=_Raw} ->
            % RawStr = exmpp_xml:document_to_iolist(_Raw),
            % error_logger:info_msg("success when trying to ~s: ~s~n", [Action, RawStr]),
            ok;
        #received_packet{packet_type=iq, type_attr="error", raw_packet=Raw} ->
            RawStr = exmpp_xml:document_to_iolist(Raw),
            error_logger:error_msg("error when trying to ~s: ~s~n", [Action, RawStr]),
            exit(failed_to_register)
        after 1000 ->
            exit(timeout)
    end.
