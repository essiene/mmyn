-module(esmetx).
-include("simreg.hrl").
-behaviour(gen_esme34).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        handle_tx/2,
        handle_rx/2,
        terminate/2,
        code_change/3]).

-export([start_link/1, start/1, stop/1, wake/1, check_and_send/1]).

-define(BK_OFF_MAX, 360000).
-define(BK_OFF_MIN, 100).
-define(BK_OFF_GROW, 1000).
-define(TXQ_CHK, txq_chk).

-record(st, {host, port, system_id, password, id, esmetx_backoff, awake}).

start_link(Id) ->
    gen_esme34:start_link(?MODULE, [Id], []).

start(Id) ->
    gen_esme34:start(?MODULE, [Id], []).

stop(Pid) ->
    gen_esme34:cast(Pid, stop).

wake(Pid) ->
    gen_esme34:cast(Pid, wake).

check_and_send(Pid) ->
    gen_esme34:cast(Pid, check_and_send).

init([Id]) ->
    {Host, Port, SystemId, Password} = util:smsc_params(),
    {ok, {Min, Max, Delta}} = application:get_env(esmetx_backoff),
	Mfa = {?MODULE, check_and_send, [self()]},
	Backoff = {Min, Max, Delta, Mfa},
    ok = backoff:register(Min, Max, Delta, Mfa),
    error_logger:info_msg("[~p] Transmitter ~p registered with backoff~n", [self(), Id]),

    {ok, {Host, Port, 
            #bind_transmitter{system_id=SystemId, password=Password}}, 
            #st{host=Host, port=Port, system_id=SystemId, password=Password,
                id=Id, esmetx_backoff=Backoff, awake=false}}.

handle_tx(Pdu, St) ->
	{tx, {?ESME_ROK, Pdu}, St}.

handle_rx(Pdu, #st{id=Id}=St) ->
    error_logger:info_msg("[~p] Transmitter ~p has received PDU: ~p~n", [self(), Id, Pdu]),
    {noreply, St}.
    
handle_call(Req, _From, St) ->
    {reply, {error, Req}, St}.

handle_cast(wake, #st{awake=false, esmetx_backoff={Min,Max,Delta,Mfa}}=St) ->
    ok = backoff:regular(Min, Max, Delta, Mfa),
    {noreply, St#st{awake=true}};

handle_cast(wake, #st{awake=true}=St) ->
    {noreply, St};

handle_cast(stop, #st{id=Id}=St) ->
    backoff:deregister(),
    error_logger:info_msg("[~p] Transmitter ~p has deregistered from backoff~n", [self(), Id]),
    {stop, normal, St#st{awake=false}};

handle_cast(check_and_send, #st{id=Id,
		esmetx_backoff={Min,Max,Delta,Mfa}}=St) ->
    case txq:pop() of 
        '$empty' ->
            error_logger:info_msg("[~p] Transmitter ~p found no tx req to send~n", [self(), Id]),
            ok = backoff:increment(Min,Max,Delta,Mfa),
            {noreply, St#st{awake=false}};
        #txq_req{src=Src, dst=Dest, message=Msg} ->
			gen_esme34:transmit_pdu(self(), #submit_sm{source_addr=Src, destination_addr=Dest, short_message=Msg}),
            error_logger:info_msg("[~p] Transmitter ~p has sent submit_sm req: ~p ~p ~p~n", [self(), Id, Src, Dest, Msg]),
            ok = backoff:regular(Min,Max,Delta,Mfa),
            {noreply, St#st{awake=true}}
    end;

handle_cast(_Req, St) ->
    {noreply, St}.

handle_info(Req, #st{id=Id}=St) ->
    error_logger:info_msg("[~p] Transmitter ~p has recieved a non gen_server request: ~p", [self(), Id, Req]),
    {noreply, St}.

terminate(Reason, #st{id=Id}) ->
    error_logger:info_msg("[~p] Transmitter ~p is terminating with reason: ~p~n", [self(), Id, Reason]),
    ok.

code_change(_OldVsn, St, _Extra) ->
    {noreply, St}.
