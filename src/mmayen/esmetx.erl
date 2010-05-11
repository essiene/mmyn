-module(esmetx).
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

-export([start_link/1, start/1, stop/1, wake/1, check_and_send/1]).

-define(BK_OFF_MAX, 360000).
-define(BK_OFF_MIN, 100).
-define(BK_OFF_GROW, 1000).
-define(TXQ_CHK, txq_chk).

-record(st, {host, port, system_id, password, smpp, id, esmetx_backoff}).

start_link(Id) ->
    gen_esme:start_link(?MODULE, [Id], []).

start(Id) ->
    gen_esme:start(?MODULE, [Id], []).

stop(Pid) ->
    gen_esme:cast(Pid, stop).

wake(Pid) ->
    gen_esme:cast(Pid, wake).

check_and_send(Pid) ->
    gen_esme:cast(Pid, check_and_send).

init([Id]) ->
    {Host, Port, SystemId, Password} = util:smsc_params(),
    {ok, Backoff} = application:get_env(esmetx_backoff),
    {ok, {Host, Port, 
            #bind_transmitter{system_id=SystemId, password=Password}}, 
            #st{host=Host, port=Port, system_id=SystemId, password=Password,
                id=Id, esmetx_backoff=Backoff}}.

handle_bind(Smpp, #st{id=Id, esmetx_backoff={Min, Max, Delta}}=St) ->
    error_logger:info_msg("[~p] Transmitter ~p bound~n", [self(), Id]),
    ok = backoff:register(Min, Max, Delta, {?MODULE, check_and_send, [self()]}),
    error_logger:info_msg("[~p] Transmitter ~p registered with backoff~n", [self(), Id]),
    {noreply, St#st{smpp=Smpp}}.

handle_pdu(Pdu, #st{id=Id}=St) ->
    error_logger:info_msg("[~p] Transmitter ~p has received PDU: ~p~n", [self(), Id, Pdu]),
    {noreply, St}.
    
handle_unbind(_Pdu, St) ->
    {noreply, St}.

handle_call(Req, _From, St) ->
    {reply, {error, Req}, St}.

handle_cast(wake, St) ->
    ok = backoff:regular(),
    {noreply, St};

handle_cast(stop, #st{id=Id}=St) ->
    backoff:deregister(),
    error_logger:info_msg("[~p] Transmitter ~p has deregistered from backoff~n", [self(), Id]),
    {stop, normal, St};

handle_cast(check_and_send, #st{smpp=Smpp, id=Id}=St) ->
    case txq:pop() of 
        '$empty' ->
            error_logger:info_msg("[~p] Transmitter ~p found no tx req to send~n", [self(), Id]),
            ok = backoff:increment();
        #txq_req{src=Src, dst=Dest, message=Msg} ->
            smpp:send(Smpp, #submit_sm{source_addr=Src, destination_addr=Dest, short_message=Msg}),
            error_logger:info_msg("[~p] Transmitter ~p has sent tx req~n", [self(), Id]),
            ok = backoff:regular()
    end,
    {noreply, St};

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
