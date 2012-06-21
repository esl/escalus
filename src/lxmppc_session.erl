%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module providing basic session manipulation
%%% @end
%%%===================================================================

-module(lxmppc_session).
-export([start_stream/2,
         authenticate/2,
         starttls/2,
         bind/2,
         compress/2,
         session/2]).

-include_lib("exml/include/exml.hrl").

-compile([export_all]).

%%%===================================================================
%%% Public API
%%%===================================================================

start_stream(Conn, Props) ->
    %%%'
    dbg:tracer(),
    dbg:p(all,call),
    %dbg:tpl(?MODULE, get_stream_features, x),
    %dbg:tpl(?MODULE, get_compression, x),
    %dbg:tpl(lxmppc_socket_tcp, use_zlib, x),
    %dbg:tpl(zlib, x),
    dbg:tp(zlib, deflateInit, x),
    dbg:tp(zlib, deflate, x),
    dbg:tp(zlib, deflateEnd, x),
    dbg:tp(zlib, inflateInit, x),
    dbg:tp(zlib, inflate, x),
    dbg:tp(zlib, inflateEnd, x),
    dbg:tpl(lxmppc_socket_tcp, strange_fun, x),
    %%%.
    Server = proplists:get_value(server, Props, <<"localhost">>),
    Host = proplists:get_value(host, Props, Server),
    ok = lxmppc:send(Conn, lxmppc_stanza:stream_start(Server)),
    StreamStart = lxmppc_util:get_stanza(Conn, wait_for_stream),
    %% FIXME: verify StreamStart
    StreamFeatures = lxmppc_util:get_stanza(Conn, wait_for_features),
    %% FIXME: verify StreamFeatures
    get_stream_features(StreamFeatures, Props).

starttls(Conn, Props) ->
    case proplists:get_value(ssl, Props, false) of
        false ->
            {Conn, Props};
        _ ->
            lxmppc_socket_tcp:upgrade_to_tls(Conn, Props)
    end.

authenticate(Conn, Props) ->
    %% FIXME: as default, select authentication scheme based on stream features
    {M, F} = proplists:get_value(auth, Props, {lxmppc_auth, auth_plain}),
    M:F(Conn, Props),
    lxmppc:reset_parser(Conn),
    lxmppc_session:start_stream(Conn, Props),
    Props.

bind(Conn, Props) ->
    Resource = proplists:get_value(resource, Props, <<"lxmppc-resource">>),
    lxmppc:send(Conn, lxmppc_stanza:bind(Resource)),
    BindReply = lxmppc_util:get_stanza(Conn, bind_reply),
    %% FIXME: verify BindReply, add props
    Props.

compress(Conn, Props) ->
    case proplists:get_value(compression, Props, false) of
        false ->
            Props;
        [<<"zlib">>] ->
            lxmppc_socket_tcp:use_zlib(Conn, Props)
        %% TODO: someday maybe lzw too
    end.

session(Conn, Props) ->
    lxmppc:send(Conn, lxmppc_stanza:session()),
    SessionReply = lxmppc_util:get_stanza(Conn, session_reply),
    %% FIXME: verify SessionReply, add props
    Props.

%%%===================================================================
%%% Helpers
%%%===================================================================

get_stream_features(Features, Props) ->
    lists:keystore(compression, 1, Props,
                   {compression, get_compression(Features)}).

get_compression(Features) ->
    #xmlelement{body = MethodEls} = exml_query:subelement(Features,
                                                          <<"compression">>),
    Methods = [ exml_query:cdata(MethodEl) || MethodEl <- MethodEls ].
