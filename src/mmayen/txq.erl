-module(txq).
-behaviour(gen_server).
-define(LOGGER, '__txq_logger').
-include("mmyn.hrl").
-export([init/1,handle_call/3,
        handle_cast/2,handle_info/2,
        terminate/2,code_change/3]).

-export([start_link/0, push/1, pop/0, ping/0, log/6]).

-record(st, {q}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push(#txq_req{}=Item) ->
    gen_server:call(?MODULE, {push, Item}).

pop() ->
    gen_server:call(?MODULE, pop).

ping() ->
	gen_server:call(?MODULE, ping).


init([]) ->
    {ok, LogDir} = application:get_env(qlog_logdir),
    {ok, LogSize} = application:get_env(qlog_logsize),
    {ok, NumRotations} = application:get_env(qlog_logkeep),
    LogFile = "txqueue",
    Suffix = "log",

    AddAppender = fun () ->
        case spq:open('.txq.q') of
            {ok, Q} ->
                case log4erl:add_file_appender(?LOGGER, file_logger_txqlog, {LogDir, LogFile, {size, LogSize}, NumRotations, Suffix, all, "%l%n"}) of
                    {ok, _} ->
                        {ok, #st{q=Q}};
                    {error, {already_started, _}} ->
                        {ok, #st{q=Q}};
                    {error, Reason} ->
                        {stop, Reason}
                end;
            {error, Reason} ->
                {stop, Reason}
        end
    end,

    case log4erl:add_logger(?LOGGER) of
        {ok, _} ->
            AddAppender();
        {error, {already_started, _}} ->
            AddAppender();
        {error, Reason} ->
            {stop, Reason}
    end.


handle_call({push, #txq_req{}=Item}, _F, #st{q=Q}=St) ->
    Qid = qid(),
    spq:push(Q, Item#txq_req{t1=now(), id=Qid}),
    {reply, {ok, Qid}, St};

handle_call(pop, _F, #st{q=Q}=St) ->
    case spq:pop(Q) of
        {error, empty} -> 
            {reply, '$empty', St};
        {value, V} ->
            {reply, V, St}
    end;

handle_call(ping, _F, #st{q=Q}=St) ->
	Len = spq:len(Q),
	{reply, {pong, [{len, Len}]}, St};

handle_call(R, _F, St) ->
    {reply, {error, {illegal_request, R}}, St}.

handle_cast(_R, St) ->
    {noreply, St}.

handle_info(_R, St) ->
    {noreply, St}.

terminate(_R, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


qid() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    lists:flatten(io_lib:format("~6.10.0B~6.10.0B~6.10.0B", [MegaSecs, Secs, MicroSecs])).


log(#txq_req{id=Tid, src=Src, dst=Dst, message=Msg0, module=Mod}, TxId, Tq, Tsend, Status, StatusDetail) ->
    Tstmp0 = calendar:now_to_local_time(now()),
    Tstmp = httpd_util:rfc1123_date(Tstmp0),

	Msg = util:replace(Msg0, "\n", "+"),
	Pid = self(),
	Log = lists:flatten(io_lib:format("~s|~.2f|~.2f|~p|~p|~p|~s|~s|~s|~s|~p|~p", [Tstmp, Tq, Tsend, TxId, Pid, Mod, Tid, Src, Dst, Msg, Status, StatusDetail])),
	ok = log4erl:log(?LOGGER, debug, "~s", [Log]).
