-module(esmerx).
-behaviour(gen_esme34).
-include("simreg.hrl").
-include("tlog.hrl").

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        handle_rx/2,
		handle_tx/3,
        terminate/2,
        code_change/3]).

-export([start_link/1, start/1, stop/1]).

-record(st, {host, port, system_id, password, callback, id,
        notify_msisdns, notify_sender}).
-record(cb, {mod, st, ready=false}).


start_link(Id) ->
    gen_esme34:start_link(?MODULE, [simreg_services, Id], []).

start(Id) ->
    gen_esme34:start(?MODULE, [?SMSC_HOST, ?SMSC_PORT, ?SYSTEM_ID, ?PASSWORD, simreg_services, Id], []).

stop(Pid) ->
    gen_esme34:cast(Pid, stop).

init([CbMod, Id]) ->
    {Host, Port, SystemId, Password} = util:smsc_params(),
    {NotifyMsisdns, NotifySender} = util:notify_params(),

    case CbMod:init() of
        {ok, CbState} ->
    		{ok, 
				{Host, Port, #bind_receiver{system_id=SystemId, password=Password}}, 
				#st{host=Host, port=Port, system_id=SystemId, password=Password, 
					callback=#cb{mod=CbMod, st=CbState, ready=true}, id=Id, notify_msisdns=NotifyMsisdns, 
					notify_sender=NotifySender}};
        {stop, Reason} ->
			{stop, Reason}
    end.

handle_tx(_, _, St) ->
	{noreply, St}. 


handle_rx(#pdu{sequence_number=Snum, body=#deliver_sm{source_addr=Src, destination_addr=Dst, short_message=Msg}}=Pdu, 
        #st{callback=#cb{mod=CbMod, st=CbSt, ready=true}=Cb, id=Id}=St) ->
    error_logger:info_msg("[~p] Receiver ~p has received PDU: ~p~n", [self(), Id, Pdu]),
    {ok, WordList} = preprocess(Msg),

    Tid = log_req(St, Pdu),

	DeliverSmResp = #deliver_sm_resp{message_id=Tid},

    case CbMod:handle_sms(Tid, Src, Dst, WordList, Pdu, CbSt) of
       {noreply, Status, CbSt1} ->
            log_status(Tid, Status),
            notify(St, Status),
            {tx, {?ESME_ROK, Snum, DeliverSmResp}, St#st{callback=Cb#cb{st=CbSt1}}};
        {reply, Reply, Status, CbSt1} ->
            log_status(Tid, {Dst, Src, Reply}, Status),
            send(Dst, Src, Reply),
            notify(St, Status),
            {tx, {?ESME_ROK, Snum, DeliverSmResp}, St#st{callback=Cb#cb{st=CbSt1}}}
    end;

    

handle_rx(Pdu, #st{id=Id}=St) ->
    error_logger:info_msg("[~p] Receiver ~p has received PDU: ~p~n", [self(), Id, Pdu]),
    {noreply, St}.
    
handle_call(Req, _From, St) ->
    {reply, {error, Req}, St}.

handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(_Req, St) ->
    {noreply, St}.

handle_info(_, St) ->
    {noreply, St}.

terminate(Reason, #st{callback=#cb{mod=CbMod, st=CbSt, ready=true}}) ->
    CbMod:terminate(Reason,CbSt),
    ok;
terminate(_, _) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {noreply, St}.

preprocess(Msg0) ->
    Msg = string:strip(Msg0),
    Lower = string:to_lower(Msg),
    {ok, string:tokens(Lower, "\n\t ")}.

send(_, _, {From, To, Reply}) ->
    sms:send(From, To, Reply, esmerx);

send(_, To, {From, Reply}) ->
    sms:send(From, To, Reply, esmerx);

send(From, To, Reply) ->
    sms:send(From, To, Reply, esmerx).

notify(_, ok) ->
    ok;
notify(_, {ok, _}) ->
    ok;
notify(#st{notify_msisdns=MsisdnList, notify_sender=Src}, Error) ->
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
