%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Module supporting various authentication mechanisms
%%% @end
%%%===================================================================
-module(escalus_auth).

%% Public APi
-export([auth_plain/2,
         auth_digest_md5/2,
         auth_sasl_anon/2,
         auth_sasl_external/2,
         auth_sasl_scram_sha1/2,
         auth_sasl_scram_sha224/2,
         auth_sasl_scram_sha256/2,
         auth_sasl_scram_sha384/2,
         auth_sasl_scram_sha512/2,
         auth_sasl_scram_sha1_plus/2,
         auth_sasl_scram_sha224_plus/2,
         auth_sasl_scram_sha256_plus/2,
         auth_sasl_scram_sha384_plus/2,
         auth_sasl_scram_sha512_plus/2,
         auth_sasl_oauth/2]).

%% Useful helpers for writing own mechanisms
-export([get_challenge/2,
         wait_for_success/2]).

%% Some shorthands
-type client() :: escalus_connection:client().
-type user_spec() :: escalus_users:user_spec().
-type hash_type() :: scram:hash_type().
-type plus_variant() :: none | tls_unique.

-include_lib("exml/include/exml.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

-spec auth_plain(client(), user_spec()) -> ok.
auth_plain(Conn, Props) ->
    Username = get_property(username, Props),
    Password = get_property(password, Props),
    Payload = <<0:8,Username/binary,0:8,Password/binary>>,
    Stanza = escalus_stanza:auth(<<"PLAIN">>, [base64_cdata(Payload)]),
    ok = escalus_connection:send(Conn, Stanza),
    wait_for_success(Username, Conn).

-spec auth_digest_md5(client(), user_spec()) -> ok.
auth_digest_md5(Conn, Props) ->
    ok = escalus_connection:send(Conn, escalus_stanza:auth(<<"DIGEST-MD5">>)),
    ChallengeData = get_challenge(Conn, challenge1),
    Response = md5_digest_response(ChallengeData, Props),
    ResponseStanza1 = escalus_stanza:auth_response([Response]),
    ok = escalus_connection:send(Conn, ResponseStanza1),
    [{<<"rspauth">>, _}] = get_challenge(Conn, challenge2), %% TODO: validate
    ResponseStanza2 = escalus_stanza:auth_response(),
    ok = escalus_connection:send(Conn, ResponseStanza2),
    wait_for_success(get_property(username, Props), Conn).

%% SCRAM Regular
-spec auth_sasl_scram_sha1(client(), user_spec()) -> ok.
auth_sasl_scram_sha1(Conn, Props) ->
    auth_sasl_scram_sha(sha, <<"SCRAM-SHA-1">>, Conn, Props).

-spec auth_sasl_scram_sha224(client(), user_spec()) -> ok.
auth_sasl_scram_sha224(Conn, Props) ->
    auth_sasl_scram_sha(sha224, <<"SCRAM-SHA-224">>, Conn, Props).

-spec auth_sasl_scram_sha256(client(), user_spec()) -> ok.
auth_sasl_scram_sha256(Conn, Props) ->
    auth_sasl_scram_sha(sha256, <<"SCRAM-SHA-256">>, Conn, Props).

-spec auth_sasl_scram_sha384(client(), user_spec()) -> ok.
auth_sasl_scram_sha384(Conn, Props) ->
    auth_sasl_scram_sha(sha384, <<"SCRAM-SHA-384">>, Conn, Props).

-spec auth_sasl_scram_sha512(client(), user_spec()) -> ok.
auth_sasl_scram_sha512(Conn, Props) ->
    auth_sasl_scram_sha(sha512, <<"SCRAM-SHA-512">>, Conn, Props).

-spec auth_sasl_scram_sha(hash_type(), binary(), client(), user_spec()) -> ok.
auth_sasl_scram_sha(HashMethod, XMPPMethod, Conn, Props) ->
    auth_sasl_scram_sha(none, HashMethod, XMPPMethod, Conn, Props).

%% SCRAM PLUS
-spec auth_sasl_scram_sha1_plus(client(), user_spec()) -> ok.
auth_sasl_scram_sha1_plus(Conn, Props) ->
    auth_sasl_scram_sha(tls_unique, sha, <<"SCRAM-SHA-1">>, Conn, Props).

-spec auth_sasl_scram_sha224_plus(client(), user_spec()) -> ok.
auth_sasl_scram_sha224_plus(Conn, Props) ->
    auth_sasl_scram_sha(tls_unique, sha224, <<"SCRAM-SHA-224">>, Conn, Props).

-spec auth_sasl_scram_sha256_plus(client(), user_spec()) -> ok.
auth_sasl_scram_sha256_plus(Conn, Props) ->
    auth_sasl_scram_sha(tls_unique, sha256, <<"SCRAM-SHA-256">>, Conn, Props).

-spec auth_sasl_scram_sha384_plus(client(), user_spec()) -> ok.
auth_sasl_scram_sha384_plus(Conn, Props) ->
    auth_sasl_scram_sha(tls_unique, sha384, <<"SCRAM-SHA-384">>, Conn, Props).

-spec auth_sasl_scram_sha512_plus(client(), user_spec()) -> ok.
auth_sasl_scram_sha512_plus(Conn, Props) ->
    auth_sasl_scram_sha(tls_unique, sha512, <<"SCRAM-SHA-512">>, Conn, Props).

-spec auth_sasl_scram_sha(plus_variant(), hash_type(), binary(), client(), user_spec()) -> ok.
auth_sasl_scram_sha(PlusVariant, HashMethod, XMPPMethod, Conn, Props) ->
    %% HERE WE GENERATE THE FIRST AUTH MESSAGE
    Username = get_property(username, Props),
    Nonce = base64:encode(crypto:strong_rand_bytes(16)),
    ClientFirstMessageBare = csvkv:format([{<<"n">>, Username}, {<<"r">>, Nonce}], false),
    GS2Header = scram_sha_auth_payload(PlusVariant, Props),
    Payload = <<GS2Header/binary, ClientFirstMessageBare/binary>>,
    % <<"n,,n=alicE_log_one_scram_51.215262,r=Yp/XFnLbydzdBKGe6eh9qA==">>
    % <<"p=tls-unique,,n=alicE_log_one_scram_51.215262,r=Yp/XFnLbydzdBKGe6eh9qA==">>
    Stanza = escalus_stanza:auth(XMPPMethod, [base64_cdata(Payload)]),

    %% Send <auth> stanza
    ok = escalus_connection:send(Conn, Stanza),

    %% Receive <challenge> stanza
    {Response, SaltedPassword, AuthMessage} =
        scram_sha_response(PlusVariant, HashMethod, Conn, GS2Header, ClientFirstMessageBare, Props),

    %% Send <response> stanza
    ResponseStanza = escalus_stanza:auth_response([Response]),
    ok = escalus_connection:send(Conn, ResponseStanza),

    %% Receive (hopefully) a <success> stanza
    AuthReply = escalus_connection:get_stanza(Conn, auth_reply),
    case AuthReply of
        #xmlel{name = <<"success">>, children = [#xmlcdata{content = CData}]} ->
            V = get_property(<<"v">>, csvkv:parse(base64:decode(CData))),
            Decoded = base64:decode(V),
            ok = scram_sha_validate_server(HashMethod, SaltedPassword, AuthMessage, Decoded);
        #xmlel{name = <<"failure">>} ->
            throw({auth_failed, Username, AuthReply})
    end.

-spec auth_sasl_anon(client(), user_spec()) -> ok.
auth_sasl_anon(Conn, Props) ->
    Stanza = escalus_stanza:auth(<<"ANONYMOUS">>),
    ok = escalus_connection:send(Conn, Stanza),
    wait_for_success(get_property(username, Props), Conn).

-spec auth_sasl_external(client(), user_spec()) -> ok.
auth_sasl_external(Conn, Props) ->
    RequestedName = get_requested_name(Props),
    Stanza = escalus_stanza:auth(<<"EXTERNAL">>, [RequestedName]),
    ok = escalus_connection:send(Conn, Stanza),
    wait_for_success(RequestedName, Conn).

-spec auth_sasl_oauth(client(), user_spec()) -> {ok, user_spec()}.
auth_sasl_oauth(Conn, Props) ->
    Token = get_property(oauth_token, Props),
    Children = [#xmlcdata{content = base64:encode(Token)}],
    Stanza = escalus_stanza:auth(<<"X-OAUTH">>, Children),
    ok = escalus_connection:send(Conn, Stanza),
    AuthReply = escalus_connection:get_stanza(Conn, auth_reply),
    NewProps = case AuthReply of
                   #xmlel{name = <<"success">>, children = ChildrenRecvd} ->
                       ([ {oauth_returned_token, base64:decode(CData)}
                          || #xmlcdata{content = CData} <- ChildrenRecvd ]
                        ++ Props);
                   #xmlel{name = <<"failure">>} ->
                       throw({auth_failed, AuthReply})
               end,
    {ok, NewProps}.

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
    CNonce = base16:encode(crypto:strong_rand_bytes(16)),
    Realm = proplists:get_value(<<"realm">>, ChallengeData, <<>>),
    QOP = get_property(<<"qop">>, ChallengeData),
    NC = <<"00000001">>,
    ServType = <<"xmpp">>,
    DigestUri = <<"xmpp/", Server/binary>>,
    FullJid = <<Username/binary, "@", Server/binary, "/", Resource/binary>>,

    Y = crypto:hash(md5, [Username, $:, Realm, $:, Password]),
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

scram_sha_response(PlusVariant, HashMethod, Conn, GS2Headers, ClientFirstMessageBare, Props) ->
    Challenge = get_challenge(Conn, challenge1, false),
    ChallengeData = csvkv:parse(Challenge),
    Password = get_property(password, Props),
    Nonce = get_property(<<"r">>, ChallengeData),
    Iteration = binary_to_integer(get_property(<<"i">>, ChallengeData)),
    Salt = base64:decode(get_property(<<"s">>, ChallengeData)),
    SaltedPassword = scram:salted_password(HashMethod, Password, Salt, Iteration),
    ClientKey = scram:client_key(HashMethod, SaltedPassword),
    StoredKey = scram:stored_key(HashMethod, ClientKey),
    CAttribute = build_c_attribute(PlusVariant, GS2Headers, Conn),
    ClientFinalMessageWithoutProof = <<CAttribute/binary,",r=", Nonce/binary>>,
    AuthMessage = <<ClientFirstMessageBare/binary,$,,
                    Challenge/binary,$,,
                    ClientFinalMessageWithoutProof/binary>>,
    ClientSignature = scram:client_signature(HashMethod, StoredKey, AuthMessage),
    ClientProof = base64:encode(crypto:exor(ClientKey, ClientSignature)),
    ClientFinalMessage = <<ClientFinalMessageWithoutProof/binary,
                           ",p=", ClientProof/binary>>,
    {base64_cdata(ClientFinalMessage),SaltedPassword, AuthMessage}.

build_c_attribute(none, GS2Headers, _Conn) ->
    GS2Headers64 = base64:encode(GS2Headers),
    <<"c=",GS2Headers64/binary>>;
build_c_attribute(tls_unique, GS2Headers, Conn) ->
    {ok, FinishedTLS} = escalus_connection:get_tls_last_message(Conn),
    GS2Headers64 = base64:encode(GS2Headers),
    FinishedTLS64 = base64:encode(FinishedTLS),
    <<"c=",GS2Headers64/binary, FinishedTLS64/binary>>.

scram_sha_validate_server(HashMethod, SaltedPassword, AuthMessage, ServerSignature) ->
    ServerKey = scram:server_key(HashMethod, SaltedPassword),
    ServerSignatureComputed = scram:server_signature(HashMethod, ServerKey, AuthMessage),
    case ServerSignatureComputed == ServerSignature of
        true ->
            ok;
        _ ->
            false
    end.

scram_sha_auth_payload(none, _) ->
    <<"n,,">>;
scram_sha_auth_payload(tls_unique, Props) ->
    case proplists:get_value(plus, proplists:get_value(server_auth, Props)) of
        false ->
            <<"y,,">>;
        true ->
            <<"p=tls-unique,,">>
    end.

hex_md5(Data) ->
    base16:encode(crypto:hash(md5, Data)).

%%--------------------------------------------------------------------
%% Helpers - actions
%%--------------------------------------------------------------------

%% Descr is any human-readable name used only for debugging.
-spec get_challenge(client(), atom()) -> [{binary(), binary()}].
get_challenge(Conn, Descr) ->
    get_challenge(Conn, Descr, true).

get_challenge(Conn, Descr, DecodeCsvkv) ->
    Challenge = escalus_connection:get_stanza(Conn, Descr),
    case Challenge of
        #xmlel{name = <<"challenge">>, children=[#xmlcdata{content = CData}]} ->
            ChallengeData = base64:decode(CData),
            case DecodeCsvkv of
                true ->
                    csvkv:parse(ChallengeData);
                _ ->
                    ChallengeData
            end;
        _ ->
            throw({expected_challenge, got, Challenge})
    end.

%% Throws!
-spec wait_for_success(any(), client()) -> ok.
wait_for_success(Username, Conn) ->
    AuthReply = escalus_connection:get_stanza(Conn, auth_reply),
    case AuthReply#xmlel.name of
        <<"success">> ->
            ok;
        R when R =:= <<"failure">> orelse R =:= <<"stream:error">> ->
            throw({auth_failed, Username, AuthReply})
    end.

get_property(PropName, Proplist) ->
    case lists:keyfind(PropName, 1, Proplist) of
        {PropName, Value} ->
            Value;
        false ->
            throw({missing_property, PropName})
    end.

get_requested_name(Props) ->
    case lists:keyfind(requested_name, 1, Props) of
        false ->
            #xmlcdata{content = <<"=">>};
        {requested_name, Value} ->
            base64_cdata(Value)
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

base64_cdata(Payload) ->
    #xmlcdata{content = base64:encode(Payload)}.
