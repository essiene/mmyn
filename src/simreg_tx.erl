-module(simreg_tx).
-include("simreg.hrl").
-behaviour(gen_esme).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        handle_bind/2,
        handle_unbind/2,
        handle_pdu/2,
        terminate/2,
        code_change/3]).

-export([start_link/1, start/1, stop/1, wake/1]).

-define(WAIT_MAX, 360000).
-define(WAIT_MIN, 100).
-define(WAIT_GROW, 1000).
-define(TXQ_CHK, txq_chk).

-record(st, {host, port, system_id, password, smpp, wait, wait_ref, id}).

start_link(Id) ->
    gen_esme:start_link(?MODULE, [Id], []).

start(Id) ->
    gen_esme:start(?MODULE, [Id], []).

stop(Pid) ->
    gen_esme:cast(Pid, stop).

wake(Pid) ->
    gen_esme:cast(Pid, wake).

init([Id]) ->
    {Host, Port, SystemId, Password} = util:smsc_params(),
    {ok, {Host, Port, 
            #bind_transmitter{system_id=SystemId, password=Password}}, 
            #st{host=Host, port=Port, system_id=SystemId, password=Password,
                wait=?WAIT_MIN, id=Id}}.

handle_bind(Smpp, #st{id=Id}=St0) ->
    error_logger:info_msg("Transmitter ~p bound. Smpp: ~p~n", [Id, Smpp]),
    St1 = wait(St0),
    {noreply, St1#st{smpp=Smpp}}.

handle_pdu(Pdu, #st{id=Id}=St) ->
    error_logger:info_msg("Transmitter ~p has received PDU: ~p~n", [Id, Pdu]),
    {noreply, St}.
    
handle_unbind(_Pdu, St) ->
    {noreply, St}.

handle_call(Req, _From, St) ->
    {reply, {error, Req}, St}.

handle_cast(wake, St) ->
    wait_normal(St);
handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(_Req, St) ->
    {noreply, St}.

handle_info(?TXQ_CHK, #st{smpp=Smpp, id=Id}=St) ->
    error_logger:info_msg("Transmitter ~p is awake~n", [Id]),
    case txq:pop() of 
        '$empty' ->
            wait_grow(St);
        #txq_req{src=Src, dst=Dest, message=Msg} ->
            smpp:send(Smpp, #submit_sm{source_addr=Src, destination_addr=Dest, short_message=Msg}),
            wait_normal(St)
    end;

handle_info(_Req, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {noreply, St}.

wait(#st{wait=N, wait_ref=undefined, id=Id}=St) ->
    error_logger:info_msg("Transmitter ~p goint to sleep. Will awake in ~p ms~n", [Id, N]),
    {ok, TRef} = timer:send_after(N, ?TXQ_CHK),
    St#st{wait_ref=TRef};
wait(#st{wait_ref=TRef}=St) ->
    timer:cancel(TRef),
    wait(St#st{wait_ref=undefined}).


wait_grow(#st{wait=Wait}=St) ->
    case Wait + ?WAIT_GROW of
        N when N < ?WAIT_MAX ->
            wait(N),
            {noreply, St#st{wait=N}};
        N ->
            wait(N),
            {noreply, St#st{wait=?WAIT_MAX}}
    end.

wait_normal(St) ->
    wait(?WAIT_MIN),
    {noreply, St#st{wait=?WAIT_MIN}}.
