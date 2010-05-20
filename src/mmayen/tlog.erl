-module(tlog).
-behaviour(gen_server).
-include("tlog.hrl").
-define(LOGGER, '__transaction_logger').

-export([init/1,handle_call/3,handle_cast/2,
        handle_info/2,terminate/2,code_change/3]).

-export([start_link/0, req/6, status/2]).


-record(st, {tbl}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

req(Host, Port, SystemId, RxId, CallbackModule, #req{}=Req0) ->
    Req = Req0#req{datetime=dtstmp()},
    Tlog = #tlog{
        tid=tid(),
        node=node(),
        smsc=Host,
        port=Port,
        system_id=SystemId,
        rxid=RxId,
        rxpid=self(),
        handler=CallbackModule,
        req=Req},
    gen_server:handle_call(?MODULE, {req, Tlog}).

status(Tid, #res{}=Res) ->
    gen_server:handle_call(?MODULE, {status, Tid, now(), Res}).



init([]) ->
    {ok, BinLog} = application:get_env(tlog_binlog),
    {ok, LogDir} = application:get_env(tlog_logdir),
    {ok, LogSize} = application:get_env(tlog_logsize),
    {ok, NumRotations} = application:get_env(tlog_logkeep),
    LogFile = "transaction",
    Suffix = "log",

    ok = log4erl:add_logger(?LOGGER),
    ok = log4erl:add_file_appender(?LOGGER, file_logger, {LogDir, LogFile, {size, LogSize}, NumRotations, Suffix, all, "%l%n"}),
    {ok, Tbl} = dets:open_file(?MODULE, [{file, BinLog}, {keypos, 2}]),

    error_logger:info_msg("~p started~n", [?MODULE]),
    {ok, #st{tbl=Tbl}}.


handle_call({req, #tlog{}=Tlog}, _F, #st{tbl=Tbl}=St) ->
    ok = dets:insert(Tbl, Tlog),
    {reply, ok, St};

handle_call({status, Tid, Now, #res{}=Res0}, _F, #st{tbl=Tbl}=St) ->
    case dets:lookup(Tbl, Tid) of
        [] ->
            {reply, ok, St};
        [#tlog{req=#req{datetime=DtStmp}}=Tlog0] ->
            ResponseTime = timer:now_diff(Now, DtStmp),
            Res = Res0#res{rt=ResponseTime},
            Tlog = Tlog0#tlog{res=Res},
            ok = dets:insert(Tbl, Tlog),
            {reply, ok, St};
        {error, Reason} ->
            {stop, Reason}
    end;

handle_call(R, _F, St) ->
    {reply, {error, R}, St}.


handle_cast(_R, St) ->
    {noreply, St}.


handle_info(_R, St) ->
    {noreply, St}.


terminate(_R, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


% Privates

tid() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    lists:flatten(io_lib:format("~6.10.0B~6.10.0B~6.10.0B", [MegaSecs, Secs, MicroSecs])).

dtstmp() ->
    calendar:universal_time().
