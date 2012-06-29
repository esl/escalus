%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module for generating stanzas
%%% @end
%%%===================================================================

-module(lxmppc_stanza).

-include("lxmppc.hrl").
-include_lib("exml/include/exml_stream.hrl").

%% stream - related exports
-export([stream_start/1,
         stream_end/0,
         starttls/0,
         compress/1]).

%% stanza - related exports
-export([iq/2]).
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

starttls() -> 
    #xmlelement{name = <<"starttls">>, 
                attrs=[
                    {<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-tls">>}
                ]}.

compress(Method) ->
    #xmlelement{name = <<"compress">>,
                attrs = [
                    {<<"xmlns">>, <<"http://jabber.org/protocol/compress">>}
                ],
                body = [
                    #xmlelement{name = <<"method">>, body = [exml:escape_cdata(Method)]}
                ]}.

%%--------------------------------------------------------------------
%% Stanza - related functions
%%--------------------------------------------------------------------

-spec iq(binary(), [xmlterm()]) -> #xmlelement{}.
iq(Type, Body) ->
    #xmlelement{name = <<"iq">>,
                attrs=[{<<"type">>, Type},
                       {<<"id">>, id()}],
                body = Body}.

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
    base16:encode(crypto:rand_bytes(16)).
