-module(txq).
-behaviour(gen_server).
-define(LOGGER, '__txq_logger').
-include("simreg.hrl").
-export([init/1,handle_call/3,
        handle_cast/2,handle_info/2,
        terminate/2,code_change/3]).

-export([start_link/0, push/1, pop/0, ping/0, log/4]).

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
        case log4erl:add_file_appender(?LOGGER, file_logger_qlog, {LogDir, LogFile, {size, LogSize}, NumRotations, Suffix, all, "%l%n"}) of
            {ok, _} ->
    			{ok, #st{q=queue:new()}};
            {error, {already_started, _}} ->
    			{ok, #st{q=queue:new()}};
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
    Q1 = queue:in(Item#txq_req{t1=now()}, Q),
    {reply, ok, St#st{q=Q1}};

handle_call(pop, _F, #st{q=Q}=St) ->
    case queue:out(Q) of
        {empty, Q} -> 
            {reply, '$empty', St};
        {{value, V}, Q1} ->
            {reply, V, St#st{q=Q1}}
    end;

handle_call(ping, _F, #st{q=Q}=St) ->
	Len = queue:len(Q),
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


tid(T1) ->
    {MegaSecs, Secs, MicroSecs} = T1,
    lists:flatten(io_lib:format("~6.10.0B~6.10.0B~6.10.0B", [MegaSecs, Secs, MicroSecs])).


log(#txq_req{t1=T1, src=Src, dst=Dst, message=Msg0, module=Mod}, TxId, Tq, Tsend) ->
    Tstmp0 = calendar:now_to_local_time(T1),
    Tstmp = httpd_util:rfc1123_date(Tstmp0),

	Msg = util:replace(Msg0, "\n", "+"),
	Pid = self(),
	Tid = tid(T1),
	Log = lists:flatten(io_lib:format("~s|~.2f|~.2f|~p|~p|~p|~s|~s|~s|~s", [Tstmp, Tq, Tsend, TxId, Pid, Mod, Tid, Src, Dst, Msg])),
	ok = log4erl:log(?LOGGER, debug, "~s", [Log]).
