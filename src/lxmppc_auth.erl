%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Module supporting various authentication mechanisms
%%% @end
%%%===================================================================
-module(lxmppc_auth).

%% Public APi
-export([auth_plain/2,
         auth_digest_md5/2]).

%% Useful helpers for writing own mechanisms
-export([auth_stanza/2,
         get_challenge/2,
         response_stanza/1,
         wait_for_success/2]).

-include_lib("exml/include/exml.hrl").

-define(LXMPPC_XMLNS_SASL, <<"urn:ietf:params:xml:ns:xmpp-sasl">>).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

auth_plain(Conn, Props) ->
    Username = get_property(username, Props),
    Password = get_property(password, Props),
    Payload = <<0:8,Username/binary,0:8,Password/binary>>,
    Stanza = auth_stanza(<<"PLAIN">>, base64_cdata(Payload)),
    ok = lxmppc:send(Conn, Stanza),
    wait_for_success(Username, Conn).

auth_digest_md5(Conn, Props) ->
    ok = lxmppc:send(Conn, auth_stanza(<<"DIGEST-MD5">>, [])),
    ChallengeData = get_challenge(Conn, challenge1),
    Response = md5_digest_response(ChallengeData, Props),
    ok = lxmppc:send(Conn, response_stanza([Response])),
    [{<<"rspauth">>, _}] = get_challenge(Conn, challenge2), %% TODO: validate
    ok = lxmppc:send(Conn, response_stanza([])),
    wait_for_success(get_property(username, Props), Conn).

%%--------------------------------------------------------------------
%% Helpers - implementation
%%--------------------------------------------------------------------

md5_digest_response(ChallengeData, Props) ->
    %% Digest calculated via description at
    %% http://web.archive.org/web/20050224191820/http://cataclysm.cx/wip/digest-hex_md5-crash.html
    Username = get_property(username, Props),
    Password = get_property(password, Props),
    Server = get_property(server, Props),
    Resource = get_property(resource, Props),
    Nonce = get_property(<<"nonce">>, ChallengeData),
    CNonce = base16:encode(crypto:rand_bytes(16)),
    Realm = proplists:get_value(<<"realm">>, ChallengeData, <<>>),
    QOP = get_property(<<"qop">>, ChallengeData),
    NC = <<"00000001">>,
    ServType = <<"xmpp">>,
    DigestUri = <<"xmpp/", Server/binary>>,
    FullJid = <<Username/binary, "@", Server/binary, "/", Resource/binary>>,

    Y = crypto:md5([Username, $:, Realm, $:, Password]),
    HA1 = hex_md5([Y, $:, Nonce, $:, CNonce, $:, FullJid]),
    HA2 = hex_md5([<<"AUTHENTICATE:">>, DigestUri]),

    %% Digest is the Z from the description above
    Digest = hex_md5([HA1, $:, Nonce, $:, NC, $:, CNonce, $:, QOP, $:, HA2]),

    base64_cdata(csvkv:format([
        {<<"username">>, Username},
        {<<"nonce">>, Nonce},
        {<<"nc">>, NC},
        {<<"cnonce">>, CNonce},
        {<<"qop">>, QOP},
        {<<"serv-type">>, ServType},
        {<<"host">>, Server},
        {<<"digest-uri">>, DigestUri},
        {<<"response">>, Digest},
        {<<"charset">>, <<"utf-8">>},
        {<<"authzid">>, FullJid}
    ])).

hex_md5(Data) ->
    base16:encode(crypto:md5(Data)).

%%--------------------------------------------------------------------
%% Helpers - actions
%%--------------------------------------------------------------------

get_challenge(Conn, Descr) ->
    Challenge = lxmppc_util:get_stanza(Conn, Descr),
    case Challenge of
        #xmlelement{name = <<"challenge">>, body=[CData]} ->
            csvkv:parse(base64:decode(exml:unescape_cdata(CData)));
        _ ->
            throw({expected_challenge, got, Challenge})
    end.

wait_for_success(Username, Conn) ->
    AuthReply = lxmppc_util:get_stanza(Conn, auth_reply),
    case AuthReply#xmlelement.name of
        <<"success">> ->
            ok;
        <<"failure">> ->
            throw({auth_failed, Username, AuthReply})
    end.

get_property(PropName, Proplist) ->
    case lists:keyfind(PropName, 1, Proplist) of
        {PropName, Value} ->
            Value;
        false ->
            throw({missing_property, PropName})
    end.

%%--------------------------------------------------------------------
%% Helpers - stanzas
%%--------------------------------------------------------------------

auth_stanza(Mechanism, Body) ->
    #xmlelement{name = <<"auth">>,
                attrs = [{<<"xmlns">>, ?LXMPPC_XMLNS_SASL},
                         {<<"mechanism">>, Mechanism}],
                body = Body}.

response_stanza(Body) ->
    #xmlelement{name = <<"response">>,
                attrs = [{<<"xmlns">>, ?LXMPPC_XMLNS_SASL}],
                body = Body}.

%%--------------------------------------------------------------------
%% Helpers - other
%%--------------------------------------------------------------------

base64_cdata(Payload) ->
    #xmlcdata{content=base64:encode(Payload)}.
