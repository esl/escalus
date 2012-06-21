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
        ssl :: boolean(),
        rcv_pid :: pid()}).
