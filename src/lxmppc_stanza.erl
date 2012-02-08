%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module for generating stanzas
%%% @end
%%%===================================================================

-module(lxmppc_stanza).

-include("lxmppc.hrl").
-include_lib("exml/include/exml_stream.hrl").

%% stream - related exports
-export([stream_start/1, stream_end/0]).

%% stanza - related exports
-export([iq/2]).
-export([auth/1, auth_plain/2]).
-export([bind/1, session/0]).

%%--------------------------------------------------------------------
%% Stream - related functions
%%--------------------------------------------------------------------

stream_start(Server) ->
    #xmlstreamstart{name = <<"stream:stream">>, attrs=[
            {<<"to">>, Server},
            {<<"version">>, <<"1.0">>},
            {<<"xml:lang">>, <<"en">>},
            {<<"xmlns">>, <<"jabber:client">>},
            {<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>}]}.

stream_end() ->
    #xmlstreamend{name = <<"stream:stream">>}.

%%--------------------------------------------------------------------
%% Stanza - related functions
%%--------------------------------------------------------------------

-spec iq(binary(), [xmlterm()]) -> #xmlelement{}.
iq(Type, Body) ->
    #xmlelement{name = <<"iq">>,
                attrs=[{<<"type">>, Type},
                       {<<"id">>, id()}],
                body = Body}.

-spec auth(binary()) -> #xmlelement{}.
auth(Mechanism) ->
    #xmlelement{name = <<"auth">>,
                attrs = [{<<"xmlns">>, ?LXMMPPC_XMLNS_SASL},
                         {<<"mechanism">>, Mechanism}]}.

-spec auth_plain(binary(), binary()) -> #xmlelement{}.
auth_plain(Username, Password) ->
    Payload = <<0:8,Username/binary,0:8,Password/binary>>,
    #xmlelement{name = <<"auth">>,
                attrs = [{<<"xmlns">>, ?LXMMPPC_XMLNS_SASL},
                         {<<"mechanism">>, <<"PLAIN">>}],
                body = [#xmlcdata{content=base64:encode(Payload)}]}.

-spec bind(binary()) -> #xmlelement{}.
bind(Resource) ->
    NS = <<"urn:ietf:params:xml:ns:xmpp-bind">>,
    iq(<<"set">>, [
        #xmlelement{name = <<"bind">>, attrs = [{<<"xmlns">>, NS}], body=[
            #xmlelement{name = <<"resource">>, body=[exml:escape_cdata(Resource)]}
        ]}]).

-spec session() -> #xmlelement{}.
session() ->
    NS = <<"urn:ietf:params:xml:ns:xmpp-session">>,
    iq(<<"set">>, #xmlelement{name = <<"session">>, attrs = [{<<"xmlns">>, NS}]}).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec id() -> binary().
id() ->
    << <<(hex(N div 16)), (hex(N rem 16))>> || <<N>> <= crypto:rand_bytes(16) >>.

hex(N) when N < 10 ->
    N + $0;
hex(N) when N < 16 ->
    N + $a.
