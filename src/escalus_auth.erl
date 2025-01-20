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
-export([auth_sasl_scram/3,
         get_challenge/2,
         wait_for_success/2]).

%% Some shorthands
-type client() :: escalus_connection:client().
-type user_spec() :: escalus_users:user_spec().
-type hash_type() :: fast_scram:sha_type().
-type plus_variant() :: undefined | none | tls_exporter.
-type scram_options() :: #{plus_variant := plus_variant(),
                           hash_type := hash_type(),
                           xmpp_method := binary()
                          }.

-include_lib("exml/include/exml.hrl").

-define(CB_LABEL, <<"EXPORTER-Channel-Binding">>).

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
    Options = #{plus_variant => undefined, hash_type => sha, xmpp_method => <<"SCRAM-SHA-1">>},
    auth_sasl_scram(Options, Conn, Props).

-spec auth_sasl_scram_sha224(client(), user_spec()) -> ok.
auth_sasl_scram_sha224(Conn, Props) ->
    Options = #{plus_variant => undefined, hash_type => sha224, xmpp_method => <<"SCRAM-SHA-224">>},
    auth_sasl_scram(Options, Conn, Props).

-spec auth_sasl_scram_sha256(client(), user_spec()) -> ok.
auth_sasl_scram_sha256(Conn, Props) ->
    Options = #{plus_variant => undefined, hash_type => sha256, xmpp_method => <<"SCRAM-SHA-256">>},
    auth_sasl_scram(Options, Conn, Props).

-spec auth_sasl_scram_sha384(client(), user_spec()) -> ok.
auth_sasl_scram_sha384(Conn, Props) ->
    Options = #{plus_variant => undefined, hash_type => sha384, xmpp_method => <<"SCRAM-SHA-384">>},
    auth_sasl_scram(Options, Conn, Props).

-spec auth_sasl_scram_sha512(client(), user_spec()) -> ok.
auth_sasl_scram_sha512(Conn, Props) ->
    Options = #{plus_variant => undefined, hash_type => sha512, xmpp_method => <<"SCRAM-SHA-512">>},
    auth_sasl_scram(Options, Conn, Props).

%% SCRAM PLUS
-spec auth_sasl_scram_sha1_plus(client(), user_spec()) -> ok.
auth_sasl_scram_sha1_plus(Conn, Props) ->
    Options = #{plus_variant => tls_exporter, hash_type => sha,
                xmpp_method => <<"SCRAM-SHA-1-PLUS">>},
    auth_sasl_scram(Options, Conn, Props).

-spec auth_sasl_scram_sha224_plus(client(), user_spec()) -> ok.
auth_sasl_scram_sha224_plus(Conn, Props) ->
    Options = #{plus_variant => tls_exporter, hash_type => sha224,
                xmpp_method => <<"SCRAM-SHA-224-PLUS">>},
    auth_sasl_scram(Options, Conn, Props).

-spec auth_sasl_scram_sha256_plus(client(), user_spec()) -> ok.
auth_sasl_scram_sha256_plus(Conn, Props) ->
    Options = #{plus_variant => tls_exporter, hash_type => sha256,
                xmpp_method => <<"SCRAM-SHA-256-PLUS">>},
    auth_sasl_scram(Options, Conn, Props).

-spec auth_sasl_scram_sha384_plus(client(), user_spec()) -> ok.
auth_sasl_scram_sha384_plus(Conn, Props) ->
    Options = #{plus_variant => tls_exporter, hash_type => sha384,
                xmpp_method => <<"SCRAM-SHA-384-PLUS">>},
    auth_sasl_scram(Options, Conn, Props).

-spec auth_sasl_scram_sha512_plus(client(), user_spec()) -> ok.
auth_sasl_scram_sha512_plus(Conn, Props) ->
    Options = #{plus_variant => tls_exporter, hash_type => sha512,
                xmpp_method => <<"SCRAM-SHA-512-PLUS">>},
    auth_sasl_scram(Options, Conn, Props).

-spec auth_sasl_scram(scram_options(), client(), user_spec()) -> ok.
auth_sasl_scram(#{plus_variant := PlusVariant,
                  hash_type := HashMethod,
                  xmpp_method := XMPPMethod},
                Conn, Props) ->
    Username = get_property(username, Props),
    Password = get_property(password, Props),
    ChannelBinding = scram_sha_auth_payload(PlusVariant, Conn),
    {ok, ClientState1} = fast_scram:mech_new(
        #{entity => client, username => Username, hash_method => HashMethod, nonce_size => 16,
          channel_binding => ChannelBinding, auth_data => #{password => Password}}),

    {continue, ClientFirst, ClientState3} = fast_scram:mech_step(ClientState1, <<>>),
    AuthStanza = escalus_stanza:auth(XMPPMethod, [base64_cdata(ClientFirst)]),
    ok = escalus_connection:send(Conn, AuthStanza),

    Challenge = get_challenge(Conn, challenge, false),

    {continue, ClientFinal, ClientState5} = fast_scram:mech_step(ClientState3, Challenge),
    Response = escalus_stanza:auth_response([base64_cdata(ClientFinal)]),
    ok = escalus_connection:send(Conn, Response),

    Success = get_success(Conn, challenge, false),
    case fast_scram:mech_step(ClientState5, Success) of
        {ok, _, _} -> ok;
        {error, _, _} -> throw({auth_failed, Username, Success})
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
    CNonce = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
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

scram_sha_auth_payload(undefined, _) ->
    {undefined, <<>>};
scram_sha_auth_payload(none, _) ->
    {none, <<>>};
scram_sha_auth_payload(tls_exporter, Conn) ->
    {ok, [Material | _]} = escalus_connection:export_key_materials(
                       Conn, [?CB_LABEL], [no_context], [32], true),
    {<<"tls-exporter">>, Material}.

hex_md5(Data) ->
    binary:encode_hex(crypto:hash(md5, Data), lowercase).

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
            maybe_decode(CData, DecodeCsvkv);
        _ ->
            throw({expected_challenge, got, Challenge})
    end.

get_success(Conn, Descr, DecodeCsvkv) ->
    Stanza = escalus_connection:get_stanza(Conn, Descr),
    case Stanza of
        #xmlel{name = Name, children=[#xmlcdata{content = CData}]}
          when Name =:= <<"success">>; Name =:= <<"failure">> ->
            maybe_decode(CData, DecodeCsvkv);
        _ ->
            throw({expected_success, got, Stanza})
    end.

maybe_decode(CData, DecodeCsvkv) ->
    Data = base64:decode(CData),
    case DecodeCsvkv of
        true -> csvkv:parse(Data);
        _ -> Data
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
