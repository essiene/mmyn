-module(rxworker).
-behaviour(gen_server).
-include("mmyn.hrl").
-include("tlog.hrl").

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-export([start_link/1, stop/1, ping/1]).

-record(st, {id, notify_msisdns, notify_sender, async_ref, rxq_ref, batch_size}).


start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

ping(Pid) ->
    gen_server:call(Pid, ping).

init([Id]) ->
    {NotifyMsisdns, NotifySender} = util:notify_params(),
    {BatchSize} = util:rxworker_params(),
    erlang:send_after(5000, self(), timeout),
    {ok, #st{id=Id, notify_msisdns=NotifyMsisdns, notify_sender=NotifySender, batch_size=BatchSize}}.


handle_call(ping, _, #st{id=Id}=St) ->
    {reply, {ok, {Id, pong}}, St};
handle_call(Req, _From, St) ->
    {reply, {error, Req}, St}.

handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(_Req, St) ->
    {noreply, St}.

handle_info({Ref, rxq_data, DataList}, #st{id=Id, async_ref=Ref, batch_size=BatchSize}=St) ->
    {ok, NewRef} = rxq:async_pop(BatchSize,Id),
    process_req(St, DataList),
    {noreply, St#st{async_ref=NewRef}};

handle_info(timeout, #st{id=Id, batch_size=BatchSize}=St) ->
    RxqRef = erlang:monitor(process, rxq),
    {ok, Ref} = rxq:async_pop(BatchSize,Id),
    {noreply, St#st{async_ref=Ref, rxq_ref=RxqRef}};

handle_info({'DOWN', RxqRef, _, _, _}, #st{rxq_ref=RxqRef}=St) ->
    erlang:send_after(11000, self(), timeout),
    {noreply, St#st{async_ref=undefined}};

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


%% 
% status() =  
%       flag() |
%       {flag(), {operation(), code()}} |
%       {flag(), {operation(), code(), details()}} |
%       {flag(), {operation(), code(), details(), extra()}}.
%
% flag() = ok | error.
%
% operation() = atom().
%
% code() = integer().
%
% details() = iolist().
%
% extra() = iolist(). 
%
%
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

process_req(_, []) ->
    ok;
process_req(St, [H|T]) ->
    process_req(St, H),
    process_req(St, T);
process_req(St, #rxq_req{id=Qid, pdu=Pdu}=Req) ->
    case rtable:select_route(Pdu) of
        {error, route_not_found} ->
            log_req(St, Req, route_not_found),
            log_status(Qid, error);
        {error, route_denied} ->
            log_req(St, Req, route_denied),
            log_status(Qid, error);
        {ok, {soap, {Url, _, _}=Addr}, RouteData} -> 
            Handler = lists:concat(["soap+", Url]),
            log_req(St, Req, Handler),
            dispatch_req(St, Qid, RouteData, {notifysoap, call, Addr});
        {ok, {soap, Url}, RouteData} -> 
            Handler = lists:concat(["soap+", Url]),
            log_req(St, Req, Handler),
            dispatch_req(St, Qid, RouteData, {notifysoap, call, Url});
        {ok, {rest, {Url,_,_}=Addr}, RouteData} -> 
            log_req(St, Req, Url),
            dispatch_req(St, Qid, RouteData, {notifyjson, call, Addr});
        {ok, {rest, Url}, RouteData} -> 
            log_req(St, Req, Url),
            dispatch_req(St, Qid, RouteData, {notifyjson, call, Url});
        {ok, {Module, Function}, RouteData} -> 
            Handler = io_lib:format("erlang://~s/~s", [Module, Function]),
            log_req(St, Req, Handler),
            dispatch_req(St, Qid, RouteData, {Module, Function});
        {ok, {Module, Function, Args}, RouteData} when is_atom(Module) -> 
            Handler = io_lib:format("erlang://~s/~s", [Module, Function]),
            log_req(St, Req, Handler),
            dispatch_req(St, Qid, RouteData, {Module, Function, Args});
        {ok, {Url,_,_}=Addr, RouteData} when is_list(Url) -> 
            log_req(St, Req, Url),
            dispatch_req(St, Qid, RouteData, {notifyjson, call, Addr});
        {ok, Url, RouteData} -> 
            log_req(St, Req, Url),
            dispatch_req(St, Qid, RouteData, {notifyjson, call, Url})
    end.

dispatch_req(St, Qid, #route_data{from=F, to=To, keywords=Kw, 
        msg=Msg}, CallbackSpec) ->
    try callback(CallbackSpec, Qid, F, To, Kw, Msg) of
        {noreply, {error, Reason}=Status} -> 
            error_logger:error_msg("RXWORKER: ~p~n", [Reason]),
            log_status(Qid, Status), 
            % service is temporarily unavailable should be configured in config file
            send(To, F, "Service is temporarily unavailable. Please try again later."),
            notify(St, Status); 
        {noreply, Status} -> 
            log_status(Qid, Status), 
            notify(St, Status); 
        {reply, {RSrc, RDst, RMsg}, Status} -> 
            log_status(Qid, {RSrc, RDst, RMsg}, Status), 
            send(RSrc, RDst, RMsg), 
            notify(St, Status);
        _ ->
            Status = {error, {dispatch_req, 500, "Callback returned invalid reply"}},
            log_status(Qid, Status),
            send(To, F, "Service is temporarily unavailable. Please try again later."),
            notify(St, Status)
     catch 
         _:Msg -> 
             Status = {error, {dispatch_req, 500, Msg}}, 
             log_status(Qid, Status), 
             send(To, F, "Service is temporarily unavailable. Please try again later."), 
             notify(St, Status)
    end.

callback({M,F}, Tid, From, To, Kw, Msg) ->
    M:F(Tid, From, To, Kw, Msg);
callback({M,F,A}, Tid, From, To, Kw, Msg) ->
    M:F(A, Tid, From, To, Kw, Msg).
