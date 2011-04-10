-module(esmerx).
-behaviour(gen_esme34).
-include("mmyn.hrl").

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
    IgnoreVersion = case application:get_env(esme_ignore_version) of
        {ok, C} ->
            C;
        undefined ->
            false
    end,
    gen_esme34:start_link(?MODULE, [Id], [{logger, {esme_logger, [esmerx, Id]}},
            {ignore_version, IgnoreVersion}]).


stop(Pid) ->
    gen_esme34:cast(Pid, stop).

init([Id]) ->
    {Host, Port, SystemId, Password} = util:esmerx_params(),

    {ok, 
        {Host, Port, #pdu{body=#bind_receiver{system_id=SystemId, password=Password}}}, 
        #st{host=Host, port=Port, system_id=SystemId, password=Password, id=Id}
    }.

handle_tx(_, _, St) ->
	{noreply, St}. 


handle_rx(#pdu{sequence_number=Snum}=Pdu, 
          #st{id=Id, host=H, port=P, system_id=Sid}=St) -> 
          
          try rxq:push(#rxq_req{rxid=Id, pdu=Pdu, 
                      host=H, port=P, system_id=Sid}) of
              {ok, Qid} -> 
                  DeliverSmResp = #deliver_sm_resp{}, 
                  Pdu = #pdu{command_status=?ESME_ROK,
                             sequence_number=Snum,
                             body=DeliverSmResp},
                  {tx, {Pdu, Qid}, St}
          catch 
              Type:Err ->
                  error_logger:error_msg("RXQ:PUSH/1 -> ~p:~p~n", [Type,Err]),
                  Pdu = #pdu{command_status=?ESME_RX_T_APPN,
                             sequence_number=Snum,
                             body=#deliver_sm_resp{}},
                  {tx, Pdu, St}
          end;

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
