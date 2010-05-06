-module(simreg_manager).
-behaviour(gen_server).

-export([start_link/1, start/1,stop/0,status/0]).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, {rx_started}).


start_link(Count) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Count], []).

start(Count) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Count], []).

stop() ->
    gen_server:call(?MODULE, stop).

status() ->
    gen_server:call(?MODULE, status).

init([RxCount]) ->
    case start_rx(RxCount) of
        {ok, 0} ->
            simreg:stop();
        {ok, Count} -> 
            {ok, #state{rx_started=Count}}
    end.

handle_call(stop, _From, St) ->
    {stop, normal, St};

handle_call(status, _From, #state{rx_started=N}=St) ->
    {ok, TxStat} = gen_esme:stat(simreg_tx),
    Children = simreg_rx_sup:children(),
    % TODO: race condition if we get a child pid and it dies before
    % we call gen_esme:stat/1
    GetRxStat = fun(Pid) -> {ok, Stat} = gen_esme:stat(Pid), Stat end,
    RxStats = lists:map(GetRxStat, Children),
    {reply, [{tx, 1}, {rx, N}, [TxStat|RxStats]], St};

handle_call(Req, _From, St) ->
    {reply, {error, Req}, St}.

handle_cast(_Req, St) ->
    {noreply, St}.

handle_info(_Req, St) ->
    {noreply, St}.

terminate(_Reason, #state{}) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {noreply, St}.

start_rx(Count) ->
    start_rx(Count, 0).

start_rx(0, C) ->
    {ok, C};
start_rx(N, C) ->
    case simreg_rx_sup:start_child() of
        {ok, _} -> 
            start_rx(N-1, C+1);
        _ ->
            start_rx(N-1, C)
    end.
