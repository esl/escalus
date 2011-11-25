%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Module for generating authentication-related stanzas
%%% @end
%%%===================================================================

-module(lxmppc_auth).

-export([plain/2]).

-include("lxmlc.hrl").
-include_lib("exml/include/exml.hrl").

%%--------------------------------------------------------------------
%% Authentication - related stanzas
%%--------------------------------------------------------------------

-spec plain(binary(), binary()) -> #xmlelement{}.
plain(Username, Password) ->
    Content = <<Username/binary,0:8,Password/binary,0:8>>,
    #xmlelement{name = <<"auth">>,
                attrs = [{<<"xmlns">>, ?LXMMPPC_XMLNS_SASL}],
                body = #xmlcdata{content=Content}}.
