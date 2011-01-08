-module(tlog).
-behaviour(gen_server).
-include("tlog.hrl").
-define(LOGGER, '__transaction_logger').

-export([init/1,handle_call/3,handle_cast/2,
        handle_info/2,terminate/2,code_change/3]).

-export([start_link/0, req/8, status/2]).


-record(st, {tbl}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

req(Tid, Host, Port, SystemId, RxId, WorkerId, Handler, #req{}=Req0) ->
    Req = Req0#req{datetime=now()},
    Tlog = #tlog{
        tid=Tid,
        node=node(),
        smsc=Host,
        port=Port,
        system_id=SystemId,
        rxid=RxId,
        wid=WorkerId,
        wpid=self(),
        handler=Handler,
        req=Req},
    gen_server:call(?MODULE, {req, Tlog}).

status(Tid, #res{}=Res) ->
    gen_server:call(?MODULE, {status, Tid, now(), Res}).



init([]) ->
    {ok, BinLog} = application:get_env(tlog_binlog),
    {ok, LogDir} = application:get_env(tlog_logdir),
    {ok, LogSize} = application:get_env(tlog_logsize),
    {ok, NumRotations} = application:get_env(tlog_logkeep),
    LogFile = "transaction",
    Suffix = "log",

    {ok, _} = log4erl:add_logger(?LOGGER),
    {ok, _} = log4erl:add_file_appender(?LOGGER, file_logger, {LogDir, LogFile, {size, LogSize}, NumRotations, Suffix, all, "%l%n"}),
    {ok, Tbl} = dets:open_file(?MODULE, [{file, BinLog}, {keypos, 2}]),

    error_logger:info_msg("~p started~n", [?MODULE]),
    {ok, #st{tbl=Tbl}}.


handle_call({req, #tlog{tid=Tid}=Tlog}, _F, #st{tbl=Tbl}=St) ->
    ok = dets:insert(Tbl, Tlog),
    {reply, Tid, St};

handle_call({status, Tid, Now, #res{}=Res0}, _F, #st{tbl=Tbl}=St) ->
    case dets:lookup(Tbl, Tid) of
        [] ->
            {reply, ok, St};
        [#tlog{req=#req{datetime=DtStmp}}=Tlog0] ->

            ResponseTime = timer:now_diff(Now, DtStmp),

            Res = Res0#res{rt=ResponseTime},
            Tlog = Tlog0#tlog{res=Res},

            TlogStr = to_string(Tlog),

            ok = log4erl:log(?LOGGER, debug, "~s", [TlogStr]),
            dets:delete(Tbl, Tlog#tlog.tid),

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

to_string(#tlog{tid=Tid, rxid=Rxid, wid=Wid, wpid=WPid, smsc=Smsc, port=Port,
        system_id=SystemId, handler=Handler, req=Req, res=Res}) ->

    #req{datetime=DateTime0, seqnum=Seqnum, src=RqSrc, dst=RqDst, msg=RqMsg0} = Req,
    #res{rt=Rt0, src=RsSrc, dst=RsDst, msg=RsMsg0, status=Status, op=Op0, code=Code0, detail=Detail, extra=Extra} = Res,

    DateTime1 = calendar:now_to_local_time(DateTime0),
    DateTime = httpd_util:rfc1123_date(DateTime1),

    ToString = fun (X) ->
        case X of
               N when is_integer(N) ->
                    integer_to_list(N);
               N ->
                 N
        end
    end,

    MicroToMilliSecs = fun (X) ->
        X/1000
    end,

    Rt = MicroToMilliSecs(Rt0),
    Op = ToString(Op0),
    Code = ToString(Code0),

	RqMsg = util:replace(RqMsg0, "\n", "+"),
	RsMsg = util:replace(RsMsg0, "\n", "+"),

    lists:flatten(io_lib:format("~s|~s|~.2f|~p|~p|~s|~s|~s|~s|~s|~s|~s|~s|~s|~s|~s|~p|~s|~b|~s|~p|~p", [Tid, DateTime, Rt, Wid, Rxid, 
            RqSrc, RqDst, RqMsg, RsSrc, RsDst, RsMsg, Handler, Status, Op, Code, Detail, 
            Extra, Smsc, Port, SystemId, Seqnum, WPid])).

