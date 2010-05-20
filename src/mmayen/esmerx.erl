-module(esmerx).
-behaviour(gen_esme).
-include("simreg.hrl").
-include("tlog.hrl").

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        handle_bind/2,
        handle_unbind/2,
        handle_pdu/2,
        terminate/2,
        code_change/3]).

-export([start_link/1, start/1, stop/1]).

-record(st, {host, port, system_id, password, smpp, callback, id,
        notify_msisdns, notify_sender}).
-record(cb, {mod, st, ready=false}).


start_link(Id) ->
    gen_esme:start_link(?MODULE, [simreg_services, Id], []).

start(Id) ->
    gen_esme:start(?MODULE, [?SMSC_HOST, ?SMSC_PORT, ?SYSTEM_ID, ?PASSWORD, simreg_services, Id], []).

stop(Pid) ->
    gen_esme:cast(Pid, stop).

init([Callback, Id]) ->
    {Host, Port, SystemId, Password} = util:smsc_params(),
    {NotifyMsisdns, NotifySender} = util:notify_params(),
    {ok, {Host, Port, 
            #bind_receiver{system_id=SystemId, password=Password}}, 
            #st{host=Host, port=Port, system_id=SystemId, password=Password,
                callback=#cb{mod=Callback}, id=Id, notify_msisdns=NotifyMsisdns,
            notify_sender=NotifySender}}.

handle_bind(Smpp, #st{id=Id, callback=#cb{mod=CbMod}=Cb}=St0) ->
    error_logger:info_msg("[~p] Receiver ~p bound~n", [self(), Id]),
    St1 = St0#st{smpp=Smpp},
    case CbMod:init() of
        {ok, CbState} ->
            {noreply, St1#st{callback=Cb#cb{st=CbState, ready=true}}};
        {stop, Reason} ->
            error_logger:error_report("[~p] Callback module ~p failed to initialize with reason: ~p~n", [self(), Reason]),
            {noreply, St1}
    end.

handle_pdu(#pdu{body=#deliver_sm{source_addr=Src, destination_addr=Dst, short_message=Msg}}=Pdu, 
        #st{callback=#cb{mod=CbMod, st=CbSt, ready=true}=Cb, id=Id}=St) ->
    error_logger:info_msg("[~p] Receiver ~p has received PDU: ~p~n", [self(), Id, Pdu]),
    {ok, WordList} = preprocess(Msg),

    error_logger:info_msg("[~p] Receiver ~p is calling callback ~p~n", [self(), Id, CbMod]),
    Tid = log_req(St, Pdu),
    case CbMod:handle_sms(Tid, Src, Dst, WordList, Pdu, CbSt) of
       {noreply, Status, CbSt1} ->
            log_status(Tid, Status),
            notify(St, Status),
            {noreply, St#st{callback=Cb#cb{st=CbSt1}}};
        {reply, Reply, Status, CbSt1} ->
            log_status(Tid, {Dst, Src, Reply}, Status),
            send(Dst, Src, Reply),
            notify(St, Status),
            {noreply, St#st{callback=Cb#cb{st=CbSt1}}}
    end;

    

handle_pdu(Pdu, #st{id=Id}=St) ->
    error_logger:info_msg("[~p] Receiver ~p has received PDU: ~p~n", [self(), Id, Pdu]),
    {noreply, St}.
    
handle_unbind(_Pdu, St) ->
    {noreply, St}.

handle_call({sendsms, Source, Dest, Msg}, _From, #st{smpp=Smpp}=St) ->
    S = smpp:send(Smpp, #submit_sm{source_addr=Source, destination_addr=Dest, short_message=Msg}),
    {reply, S, St};
handle_call(Req, _From, St) ->
    {reply, {error, Req}, St}.

handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(_Req, St) ->
    {noreply, St}.

handle_info(Req, #st{id=Id}=St) ->
    error_logger:info_msg("[~p] Receiver ~p has recieved a non gen_server request: ~p", [self(), Id, Req]),
    {noreply, St}.

terminate(Reason, #st{id=Id, callback=#cb{mod=CbMod, st=CbSt, ready=true}}) ->
    error_logger:info_msg("[~p] Receiver ~p is terminating with reason: ~p~n", [self(), Id, Reason]),
    CbMod:terminate(Reason,CbSt),
    ok;
terminate(Reason, #st{id=Id}) ->
    error_logger:info_msg("[~p] Receiver ~p is terminating with reason: ~p~n", [self(), Id, Reason]),
    ok.

code_change(_OldVsn, St, _Extra) ->
    {noreply, St}.

preprocess(Msg) ->
    Lower = string:to_lower(Msg),
    {ok, string:tokens(Lower, " ")}.

send(_, _, {From, To, Reply}) ->
    sms:send(From, To, Reply);

send(_, To, {From, Reply}) ->
    sms:send(From, To, Reply);

send(From, To, Reply) ->
    sms:send(From, To, Reply).

notify(_, ok) ->
    ok;
notify(_, {ok, _}) ->
    ok;
notify(#st{id=Id, notify_msisdns=MsisdnList, notify_sender=Src}, Error) ->
    error_logger:info_msg("[~p] Receiver ~p is going to notify: ~p => ~p (~p)~n", [self(), Id, Src, MsisdnList, Error]),
    notify(Src, MsisdnList, Error).

notify(_, undefined, _) ->
    ok;
notify(undefined, MsisdnList, Error) ->
    notify("mmyn", MsisdnList, Error);
notify(_, [], Msg) when is_list(Msg) ->
    ok;
notify(Src, [Msisdn|Rest], Msg) when is_list(Msg) ->
    send(Src, Msisdn, Msg),
    notify(Src, Rest, Msg);
notify(Src, MsisdnList, {error, {Op, Code, Message}}) ->
    notify(Src, MsisdnList, util:sms_format_msg("~p~n~p~n~p", [Op, Code, Message]));
notify(Src, MsisdnList, {error, Reason}) ->
    notify(Src, MsisdnList, util:sms_format_msg("~p", [Reason])).

log_req(#st{host=Host, port=Port, system_id=SystemId, id=Id, callback=#cb{mod=Module}},
#pdu{sequence_number=Sn, body=#deliver_sm{source_addr=From, destination_addr=To,
        short_message=Msg}}) ->

    Req = #req{
            seqnum = Sn, 
            src = From, 
            dst = To, 
            msg = Msg},

    tlog:req(Host,
            Port,
            SystemId,
            Id,
            Module,
            Req).

log_status(Tid, Status) ->
    log_status(Tid, undefined, Status).

log_status(Tid, undefined, Status) ->
    log_status(Tid, {"", "", ""}, Status);

log_status(Tid, {_, _, {From, To, Msg}}, Status) ->
    log_status(Tid, {From, To, Msg}, Status);

log_status(Tid, {_, To, {From, Msg}}, Status) ->
    log_status(Tid, {From, To, Msg}, Status);

log_status(Tid, Reply, ok) ->
    log_status(Tid, Reply, {ok, {"", "", "", ""}});

log_status(Tid, Reply, error) ->
    log_status(Tid, Reply, {error, {"", "", "", ""}});

log_status(Tid, Reply, {Status, {Op, Code}}) ->
    log_status(Tid, Reply, {Status, {Op, Code, "", ""}});

log_status(Tid, Reply, {Status, {Op, Code, Detail}}) ->
    log_status(Tid, Reply, {Status, {Op, Code, Detail, ""}});

log_status(Tid, {From, To, Msg}, {Status, {Op, Code, Detail, Extra}}) ->
    Res = #res{
            src=From, 
            dst=To, 
            msg=Msg,
            status=Status,
            op=Op,
            code=Code,
            detail=Detail,
            extra=Extra},

    tlog:status(Tid, Res).
