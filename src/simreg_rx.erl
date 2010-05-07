-module(simreg_rx).
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

-export([start_link/1, start/1, stop/1]).

-record(state, {host, port, system_id, password, smpp, callback, id}).

start_link(Id) ->
    gen_esme:start_link(?MODULE, [?SMSC_HOST, ?SMSC_PORT, ?SYSTEM_ID, ?PASSWORD, simreg_services, Id], []).

start(Id) ->
    gen_esme:start(?MODULE, [?SMSC_HOST, ?SMSC_PORT, ?SYSTEM_ID, ?PASSWORD, simreg_services, Id], []).

stop(Pid) ->
    gen_esme:cast(Pid, stop).

init([Host, Port, SystemId, Password, Callback, Id]) ->
    {ok, {Host, Port, 
            #bind_receiver{system_id=SystemId, password=Password}}, 
            #state{host=Host, port=Port, system_id=SystemId, password=Password,
                callback=Callback, id=Id}}.

handle_bind(Smpp, #state{id=Id}=St) ->
    error_logger:info_msg("Receiver ~p bound. Smpp: ~p~n", [Id, Smpp]),
    {noreply, St#state{smpp=Smpp}}.

handle_pdu(#pdu{body=#deliver_sm{source_addr=Src, destination_addr=Dst, short_message=Msg}}=Pdu, #state{callback=Callback, id=Id}=St) ->
    error_logger:info_msg("Receiver ~p received PDU: ~p~n", [Id, Pdu]),
    {ok, WordList} = preprocess(Msg),
    Callback:handle_sms(Src, Dst, WordList),
    {noreply, St};

handle_pdu(Pdu, #state{id=Id}=St) ->
    error_logger:info_msg("Receiver ~p received PDU: ~p~n", [Id, Pdu]),
    {noreply, St}.
    
handle_unbind(_Pdu, St) ->
    {noreply, St}.

handle_call({sendsms, Source, Dest, Msg}, _From, #state{smpp=Smpp}=St) ->
    S = smpp:send(Smpp, #submit_sm{source_addr=Source, destination_addr=Dest, short_message=Msg}),
    {reply, S, St};
handle_call(Req, _From, St) ->
    {reply, {error, Req}, St}.

handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(_Req, St) ->
    {noreply, St}.

handle_info(Req, #state{id=Id}=St) ->
    error_logger:info_msg("Receiver ~p recieved non gen_server request: ~p", [Id, Req]),
    {noreply, St}.

terminate(Reason, #state{id=Id}) ->
    error_logger:info_msg("Receiver ~p is terminating with reason: ~p~n", [Id, Reason]),
    ok.

code_change(_OldVsn, St, _Extra) ->
    {noreply, St}.

preprocess(Msg) ->
    Lower = string:to_lower(Msg),
    {ok, string:tokens(Lower, " ")}.
