-module(esmetx).
-include("mmyn.hrl").
-behaviour(gen_esme34).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        handle_tx/3,
        handle_rx/2,
        terminate/2,
        code_change/3]).

-export([start_link/1, stop/1, wake/1, check_and_send/1]).

-record(st, {host, port, system_id, password, id, batch_sz, batch_pending}).

start_link(Id) ->
    IgnoreVersion = case application:get_env(esme_ignore_version) of
        {ok, C} ->
            C;
        undefined ->
            false
    end,
    gen_esme34:start_link(?MODULE, [Id], [{logger, {esme_logger, [esmetx, Id]}},
            {ignore_version, IgnoreVersion}]).

stop(Pid) ->
    gen_esme34:cast(Pid, stop).

wake(Pid) ->
    gen_esme34:cast(Pid, wake).

check_and_send(Pid) ->
    gen_esme34:cast(Pid, check_and_send).

init([Id]) ->
    {Host, Port, SystemId, Password} = util:esmetx_params(),
    {PendingBatches, BatchSize} = util:esmetx_batch_params(),

    init_batch_request(PendingBatches, BatchSize),

    {ok, {Host, Port, 
            #bind_transmitter{system_id=SystemId, password=Password}}, 
            #st{host=Host, port=Port, system_id=SystemId, 
                password=Password, id=Id, batch_sz=BatchSize, 
                batch_pending=PendingBatches}}.

handle_tx({Status, StatusDetail}, {#txq_req{t1=T1}=QItem, DqTime}, #st{id=Id}=St) ->
	Qtime = time_diff(DqTime, T1),
	SendTime = time_diff(now(), DqTime),
	txq:log(QItem, Id, Qtime, SendTime, Status, StatusDetail),
	{noreply, St}.

handle_rx(_, St) ->
    % eventually log submit_sm_resp here
    {noreply, St}.
    
handle_call(Req, _From, St) ->
    {reply, {error, Req}, St}.

handle_cast(wake, #st{awake=false, esmetx_backoff={Min,Max,Delta,Mfa}}=St) ->
    ok = backoff:regular(Min, Max, Delta, Mfa),
    {noreply, St#st{awake=true}};

handle_cast(wake, #st{awake=true}=St) ->
    {noreply, St};

handle_cast(stop, #st{}=St) ->
    {stop, normal, St};

handle_cast(_Req, St) ->
    {noreply, St}.

handle_info({_, qdata, Items}, #st{}=St) ->
    transmit(St, Items),
    {noreply, St};

handle_info(_, St) ->
    {noreply, St}.

terminate(_, _) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {noreply, St}.


time_diff(T2, T1) ->
	Diff = timer:now_diff(T2, T1),
	Diff/1000.

transmit(#st{batch_sz=BatchSize}, []) ->
    txq:apop(BatchSize);
transmit(St, [QItem|Rest]) -> 
    #txq_req{src=Src, dst=Dest, message=Msg}=QItem, 
    DqTime = now(), 
    gen_esme34:transmit_pdu(self(), #submit_sm{source_addr=Src, destination_addr=Dest, short_message=Msg}, {QItem, DqTime}), 
    transmit(St, Rest).

init_batch_request(PendingBatches, BatchSize) ->
    init_batch_request(PendingBatches, BatchSize, 0).

init_batch_request(Max, _, Max) ->
    ok;
init_batch_request(Max, BatchSize, C) ->
    txq:apop(BatchSize),
    init_batch_request(Max, BatchSize, C+1).
