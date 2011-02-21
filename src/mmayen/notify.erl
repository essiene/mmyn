-module(notify).
-behaviour(gen_server).
-include("mmyn_soap.hrl").
-include("notify_soap.hrl").
-include_lib("detergent/include/detergent.hrl").

-export([start_link/0, start_link/1, call/6]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-record(st_notify, {wsdl}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(WsdlFile) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [WsdlFile], []).


call(Url, Tid, From, To, Keywords, Msg) ->
    H = #'mmyn:Header' {
        fields = #'mmyn:MmynHeader' {
            'System' = "mmyn",
            'TransactionID' = Tid
        }
    },

    R = #'mmyn:Notify' {
        fields = #'mmyn:NotifyRequest' {
            id = Tid,
            shortcode = To,
            keyword = string:join(Keywords, " "),
            msisdn = From,
            message = string:join(Msg, " "),
            'max-ttl' = 3000
        }
    },

    gen_server:call(?MODULE, {notify, Url, H, R}).





init([]) ->
    init(["var/www/notify-2.0.wsdl"]);
init([WsdlFile]) -> 
    Wsdl=detergent:initModel(WsdlFile, "mmyn"),
    {ok, #st_notify{wsdl=Wsdl}}.


handle_call({notify, Url, H, R}, _From, #st_notify{wsdl=Wsdl}=St) ->
    Reply = case detergent:call({Wsdl, Url}, "Notify", [H], [R]) of
        {error, Reason} ->
            {noreply, {error, {notify, 500, Reason}}};
        {ok, _Headers, [#'soap:Fault'{faultstring=Reason}]} ->
            {noreply, {error, {notify, 501, Reason}}};
        {ok, _Headers, [#'mmyn:Response'{fields=#'mmyn:NotifyResponse'{
                        status=0, detail=Detail}}]} ->
            {noreply, {ok, {notify, 0, Detail}}};
        {ok, _Headers, [#'mmyn:Response'{fields=#'mmyn:NotifyResponse'{
                        status=N, detail=Detail}}]} ->
            {noreply, {error, {notify, N, Detail}}}
    end,
    {reply, Reply, St};

handle_call(R, _, St) ->
    {reply, {error, R}, St}.


handle_cast(_, St) ->
    {noreply, St}.

handle_info(_, St) ->
    {noreply, St}.

terminate(_,_) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
