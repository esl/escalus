%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%%===================================================================

-record(jid, {
        user :: binary(),
        server :: binary(),
        resource = <<"">> :: binary()}).

-record(transport, {
        module :: atom(),
        socket :: term(),
        rcv_pid :: pid()}).

-define(LXMMPPC_XMLNS_SASL, <<"urn:ietf:params:xml:ns:xmpp-sasl">>).
