-module(escalus_bosh_gun).

-behaviour(gen_server).

-export([start_link/1,
         stop/1,
         request/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {destination,
                options,
                max,
                total,
                free,
                busy,
                queue
               }).
-type state() :: #state{}.

-spec start_link(list()) -> any().
start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).

stop(Pool) ->
    gen_server:cast(Pool, stop).

request(Pool, Path, Hdrs, Body) ->
    case get_client(Pool) of
        {error, _} = Error ->
            Error;
        Client ->
            StreamRef = gun:post(Client, Path, Hdrs, Body),
            Reply = wait_for_response(Client, StreamRef),
            free_client(Pool, Client),
            Reply
    end.

%% doc
%% The function returns timeout = 0
%% to immediatally set a timeout message to self
%% in order to initialise the first connection just after
%% the init function finishes
-spec init([any()]) -> {ok, state(), 0}.
init([Args]) ->
    process_flag(trap_exit, true),
    Port = proplists:get_value(port, Args, 5280),
    Host = proplists:get_value(host, Args, <<"localhost">>),
    GunOpts = gun_options(Args),
    {ok, #state{destination = {host_to_list(Host), Port},
                options = GunOpts,
                total = 0,
                max = 2,
                free = [],
                busy = [],
                queue = queue:new()
               }, 0}.

handle_call(get_client, _From, State = #state{free = [Client | Free],
                                              busy = Busy}) ->
    {reply, Client, State#state{free = Free,
                                busy = [Client | Busy]}};
handle_call(get_client, _From, State = #state{destination = Destination,
                                              options = Options,
                                              free = [],
                                              max = M,
                                              total = T,
                                              busy = Busy})
  when M > T ->
    {ok, Pid} = connect(Destination, Options),
    {reply, Pid, State#state{total = T + 1,
                             busy = [Pid | Busy]}};
handle_call(get_client, From, State = #state{free = [],
                                             max = M,
                                             total = T,
                                             queue = Queue})
  when M == T ->
    {noreply, State#state{queue = queue:in(From, Queue)}}.

handle_cast({free_client, Pid}, State = #state{free = Free,
                                                      busy = Busy,
                                                      queue = Queue}) ->
    case queue:is_empty(Queue) of
        true ->
            {noreply, State#state{free = [Pid | Free],
                                    busy = lists:delete(Pid, Busy)}};
        false ->
            {{value, From}, Q2} = queue:out(Queue),
            gen_server:reply(From, Pid),
            {noreply, State#state{queue = Q2}}
    end;
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({'EXIT', From, _Reason}, State = #state{free = Free,
                                                    busy = Busy,
                                                    total = Total}) ->
    {noreply, State#state{free = lists:delete(From, Free),
                          busy = lists:delete(From, Busy),
                          total = Total - 1}};
handle_info(timeout, #state{free = [], busy = [],
                            destination = Destination,
                            options = Options} = State) ->
    {ok, Pid} = connect(Destination, Options),
    {noreply, State#state{free = [Pid], total = 1}};
handle_info(_Info, State) ->
    ct:pal("Unknown Info in bosh_gun: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, #state{free = Free, busy = Busy}) ->
    [gun:close(F) || F <- Free],
    [gun:close(B) || B <- Busy],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

connect({Host, Port}, Options) ->
    {ok, Pid} = gun:open(Host, Port, Options#{protocols => [http]}),
    {ok, http} = gun:await_up(Pid),
    %% TODO call OnConnectFun
    {ok, Pid}.

%% Helper functions

host_to_list({_, _, _, _} = IP4) -> inet:ntoa(IP4);
host_to_list({_, _, _, _, _, _, _, _} = IP6) -> inet:ntoa(IP6);
host_to_list(BHost) when is_binary(BHost) -> binary_to_list(BHost);
host_to_list(Host) when is_list(Host) -> Host.

get_client(Pool) ->
    try
        gen_server:call(Pool, get_client)
    catch
        exit:{timeout, _} ->
            {error, timeout}
    end.

free_client(Pool, Client) ->
    gen_server:cast(Pool, {free_client, Client}).

wait_for_response(Client, StreamRef) ->
    case gun:await(Client, StreamRef) of
        {response, fin, _Status, _Headers} ->
            no_data;
        {response, nofin, _Status, _Headers} ->
            gun:await_body(Client, StreamRef)
    end.

gun_options(Args) ->
    SSLOpts = proplists:get_value(ssl_opts, Args, []),
    case proplists:get_value(ssl, Args, false) of
    true ->
        #{transport => tls,
          tls_opts => SSLOpts};
    _ ->
        #{}
end.

