-module(simreg_tx).
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

-export([start_link/0, start/0, stop/0, sendsms/3]).

-record(state, {host, port, system_id, password, smpp}).

start_link() ->
    gen_esme:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_esme:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_esme:cast(?MODULE, stop).

sendsms(Source, Dest, Msg) ->
    gen_esme:call(?MODULE, {sendsms, Source, Dest, Msg}).

init([]) ->
    {Host, Port, SystemId, Password} = util:smsc_params(),
    {ok, {Host, Port, 
            #bind_transmitter{system_id=SystemId, password=Password}}, 
            #state{host=Host, port=Port, system_id=SystemId, password=Password}}.

handle_bind(Smpp, St) ->
    {noreply, St#state{smpp=Smpp}}.

handle_pdu(Pdu, St) ->
    error_logger:info_report([simreg_tx, {unhandled_pdu, Pdu}]),
    {noreply, St}.
    
handle_unbind(_Pdu, St) ->
    {noreply, St}.

handle_call({sendsms, Source, Dest, Msg}, _From, #state{smpp=Smpp}=St) ->
    S = smpp:send(Smpp, #submit_sm{source_addr=Source, destination_addr=Dest,
            short_message=Msg}),
    {reply, S, St};
handle_call(Req, _From, St) ->
    {reply, {error, Req}, St}.

handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(_Req, St) ->
    {noreply, St}.

handle_info(_Req, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {noreply, St}.
