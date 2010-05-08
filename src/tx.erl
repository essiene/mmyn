-module(tx).
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

-define(BK_OFF_MAX, 360000).
-define(BK_OFF_MIN, 100).
-define(BK_OFF_GROW, 1000).
-define(TXQ_CHK, txq_chk).

-record(st, {host, port, system_id, password, smpp, backoff, backoff_ref, id}).

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
                backoff=?BK_OFF_MIN, id=Id}}.

handle_bind(Smpp, #st{id=Id}=St0) ->
    error_logger:info_msg("Transmitter ~p bound. Smpp: ~p~n", [Id, Smpp]),
    St1 = backoff(St0),
    {noreply, St1#st{smpp=Smpp}}.

handle_pdu(Pdu, #st{id=Id}=St) ->
    error_logger:info_msg("Transmitter ~p has received PDU: ~p~n", [Id, Pdu]),
    {noreply, St}.
    
handle_unbind(_Pdu, St) ->
    {noreply, St}.

handle_call(Req, _From, St) ->
    {reply, {error, Req}, St}.

handle_cast(wake, St) ->
    backoff_normal(St);
handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(_Req, St) ->
    {noreply, St}.

handle_info(?TXQ_CHK, #st{smpp=Smpp, id=Id}=St) ->
    error_logger:info_msg("Transmitter ~p is awake~n", [Id]),
    case txq:pop() of 
        '$empty' ->
            backoff_grow(St);
        #txq_req{src=Src, dst=Dest, message=Msg} ->
            smpp:send(Smpp, #submit_sm{source_addr=Src, destination_addr=Dest, short_message=Msg}),
            backoff_normal(St)
    end;

handle_info(_Req, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {noreply, St}.

backoff(#st{backoff=N, backoff_ref=undefined, id=Id}=St) ->
    error_logger:info_msg("Transmitter ~p goint to sleep. Will awake in ~p ms~n", [Id, N]),
    {ok, TRef} = timer:send_after(N, ?TXQ_CHK),
    St#st{backoff_ref=TRef};
backoff(#st{backoff_ref=TRef}=St) ->
    timer:cancel(TRef),
    backoff(St#st{backoff_ref=undefined}).


backoff_grow(#st{backoff=BkOff}=St) ->
    case BkOff + ?BK_OFF_GROW of
        N when N < ?BK_OFF_MAX ->
            {noreply, backoff(St#st{backoff=N})};
        _ ->
            {noreply, backoff(St#st{backoff=?BK_OFF_MAX})}
    end.

backoff_normal(St) ->
    {noreply, backoff(St#st{backoff=?BK_OFF_MIN})}.
