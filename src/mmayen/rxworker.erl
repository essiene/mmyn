-module(rxworker).
-behaviour(gen_server).
-include("simreg.hrl").
-include("tlog.hrl").

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-export([start_link/1, stop/1, ping/1]).

-record(st, {id, notify_msisdns, notify_sender, async_ref}).


start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

ping(Pid) ->
    gen_server:call(Pid, ping).

init([Id]) ->
    {NotifyMsisdns, NotifySender} = util:notify_params(),
    erlang:send_after(5000, self(), timeout),
    {ok, #st{id=Id, notify_msisdns=NotifyMsisdns, notify_sender=NotifySender}}.


handle_call(ping, _, #st{id=Id}=St) ->
    {reply, {ok, {Id, pong}}, St};
handle_call(Req, _From, St) ->
    {reply, {error, Req}, St}.

handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(_Req, St) ->
    {noreply, St}.

handle_info({Ref, rxq_data, DataList}, #st{id=Id, async_ref=Ref}=St) ->
    {ok, NewRef} = rxq:async_pop(1,Id),
    process_req(St, DataList),
    {noreply, St#st{async_ref=NewRef}};

handle_info(timeout, #st{id=Id}=St) ->
    {ok, Ref} = rxq:async_pop(1,Id),
    {noreply, St#st{async_ref=Ref}};

handle_info(_, St) ->
    {noreply, St}.

terminate(_, _) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {noreply, St}.


send(_, _, {From, To, Reply}) ->
    sms:send(From, To, Reply, rxworker);

send(_, To, {From, Reply}) ->
    sms:send(From, To, Reply, rxworker);

send(From, To, Reply) ->
    sms:send(From, To, Reply, rxworker).

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

log_req(#st{id=Id}, #rxq_req{id=Qid, rxid=RxId, host=Host, port=Port,
        system_id=SystemId, pdu= 
            #pdu{sequence_number=Sn, 
                body=#deliver_sm{source_addr=From, destination_addr=To, 
                    short_message=Msg}}}, Handler) ->

    Req = #req{
            seqnum = Sn, 
            src = From, 
            dst = To, 
            msg = Msg},

    tlog:req(Qid,
            Host,
            Port,
            SystemId,
            RxId, 
            Id,
            Handler,
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

process_req(St, []) ->
    ok;
process_req(St, [H|T]) ->
    process_req(St, H),
    process_req(St, T);
process_req(St, #rxq_req{id=Qid, pdu=Pdu}=Req) ->
    %log_req(St, Req, 'generic_handler'),
%
%    {ok, WordList} = preprocess(Msg),
%
%    case CbMod:handle_sms(Qid, Src, Dst, WordList, Pdu, CbSt) of
%       {noreply, Status, CbSt1} ->
%            log_status(Tid, Status),
%            notify(St, Status),
%            {tx, {?ESME_ROK, Snum, DeliverSmResp, Tid}, St#st{callback=Cb#cb{st=CbSt1}}};
%        {reply, Reply, Status, CbSt1} ->
%            log_status(Tid, {Dst, Src, Reply}, Status),
%            send(Dst, Src, Reply),
%            notify(St, Status),
%            {tx, {?ESME_ROK, Snum, DeliverSmResp, Tid}, St#st{callback=Cb#cb{st=CbSt1}}}
%    end.
ok.

