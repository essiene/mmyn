-module(esmerx).
-behaviour(gen_esme34).
-include("simreg.hrl").

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        handle_rx/2,
		handle_tx/3,
        terminate/2,
        code_change/3]).

-export([start_link/1, stop/1]).

-record(st, {host, port, system_id, password, id}).


start_link(Id) ->
    gen_esme34:start_link(?MODULE, [Id], [{logger, {esme_logger, [esmerx, Id]}}]).

stop(Pid) ->
    gen_esme34:cast(Pid, stop).

init([Id]) ->
    {Host, Port, SystemId, Password} = util:smsc_params(),

    {ok, 
        {Host, Port, #bind_receiver{system_id=SystemId, password=Password}}, 
        #st{host=Host, port=Port, system_id=SystemId, password=Password, id=Id}
    }.

handle_tx(_, _, St) ->
	{noreply, St}. 


handle_rx(#pdu{sequence_number=Snum}=Pdu, 
          #st{id=Id, host=H, port=P, system_id=Sid}=St) ->

    {ok, Qid} = rxq:push(#rxq_req{rxid=Id, pdu=Pdu, 
                          host=H, port=P, system_id=Sid}),
    DeliverSmResp = #deliver_sm_resp{},
    {tx, {?ESME_ROK, Snum, DeliverSmResp, Qid}, St};
    

handle_rx(_, St) ->
    {noreply, St}.
    
handle_call(Req, _From, St) ->
    {reply, {error, Req}, St}.

handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(_Req, St) ->
    {noreply, St}.

handle_info(_, St) ->
    {noreply, St}.

terminate(_, _) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {noreply, St}.
