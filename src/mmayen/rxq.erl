-module(rxq).
-behaviour(gen_server).
-define(LOGGER, '__rxq_logger').
-include("simreg.hrl").
-export([init/1,handle_call/3,
        handle_cast/2,handle_info/2,
        terminate/2,code_change/3]).

-export([start_link/0, push/1, pop/0, ping/0]).
-export([async_pop/1, asyncq/0]).

-record(st, {q, async_rx}).
-record(async_req, {sender, window_sz, ref, t1}).
-record(log, {qid, rwait, caller, cwait}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push(#rxq_req{}=Item) ->
    gen_server:call(?MODULE, {push, Item}).

pop() ->
    gen_server:call(?MODULE, {pop, self(), now()}).

async_pop(WindowSize) ->
    gen_server:cast(?MODULE, {async_pop_req, WindowSize, self(), now()}).

asyncq() ->
    gen_server:call(?MODULE, asyncq).

ping() ->
	gen_server:call(?MODULE, ping).


init([]) ->
    {ok, LogDir} = application:get_env(qlog_logdir),
    {ok, LogSize} = application:get_env(qlog_logsize),
    {ok, NumRotations} = application:get_env(qlog_logkeep),
    LogFile = "rxqueue",
    Suffix = "log",

    StartNormal = fun(Spq) ->
            erlang:send_after(2000, self(), async_pop),
            {ok, #st{q=Spq, async_rx=queue:new()}}
    end,

    AddAppender = fun () ->
        case spq:open('.rxq.q') of
            {ok, Q} ->
                case log4erl:add_file_appender(?LOGGER, file_logger_rxqlog, {LogDir, LogFile, {size, LogSize}, NumRotations, Suffix, all, "%l%n"}) of
                    {ok, _} ->
                        StartNormal(Q);
                    {error, {already_started, _}} ->
                        StartNormal(Q);
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


handle_call(asyncq, _, #st{async_rx=ARx}=St) ->
    {reply, {ok, queue:to_list(ARx)}, St};

handle_call({push, #rxq_req{}=Item}, _F, #st{q=Q}=St) ->
    Qid = qid(),
    spq:push(Q, Item#rxq_req{t1=now(), id=Qid}),
    {reply, {ok, Qid}, St};

handle_call({async_pop_req, W, S, T}, _F, #st{async_rx=AsyncRx0}=St) ->
    R = make_ref(),
    Rq = #async_req{sender=S, window_sz=W, ref=R, t1=T},
    AsyncRx1 = queue:in(Rq, AsyncRx0),
    erlang:send_after(500, self(), async_pop),
    {reply, {ok, R}, St#st{async_rx=AsyncRx1}};

handle_call({pop, Caller, Time}, _F, #st{q=Q}=St) ->
    case spq:pop(Q) of
        {error, empty} -> 
            {reply, '$empty', St};
        {value, V} ->
            log(V, Caller, Time),
            {reply, V, St}
    end;

handle_call(ping, _F, #st{q=Q, async_rx=ARx}=St) ->
	Len = spq:len(Q),
    AsyncQLen = queue:len(ARx),
	{reply, {pong, [{len, Len}, {asyncq_len, AsyncQLen}]}, St};

handle_call(R, _F, St) ->
    {reply, {error, {illegal_request, R}}, St}.

handle_cast(_R, St) ->
    {noreply, St}.

handle_info(async_pop, #st{async_rx=ARx0, q=Q}=St) ->
    {Time, ARx} = case handle_async_pop(ARx0, Q) of
        {noitem, ARx1} ->
            {2000, ARx1};
        {noreq, ARx1} ->
            {2000, ARx1};
        {ok, ARx1} ->
            {500, ARx1}
    end,
    erlang:send_after(Time, self(), async_pop),
    {noreply, St#st{async_rx=ARx}};

handle_info(_R, St) ->
    {noreply, St}.

terminate(_R, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


qid() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    lists:flatten(io_lib:format("~6.10.0B~6.10.0B~6.10.0B", [MegaSecs, Secs, MicroSecs])).

handle_async_pop(AsyncQ, Spq) ->
    case spq:len(Spq) of
        0 ->
            {noitem, AsyncQ};
        _ ->
            case queue:out(AsyncQ) of
                {empty, AsyncQ} ->
                    {noreq, AsyncQ};
                {{value, #async_req{sender=S, window_sz=W, ref=Ref, t1=T1}}, AsyncQ1} ->
                    case is_process_alive(S) of
                        false ->
                            %% log dead requester?
                            handle_async_pop(AsyncQ1, Spq);
                        true ->
                            Items = spq:pop(Spq, W),
                            log(Items, S, T1),
                            S ! {Ref, rxq_data, Items},
                            {ok, AsyncQ1}
                    end
            end
    end.


log(#rxq_req{id=Id, t1=T1}, Caller, RequestTime) ->
    T2 = now(),
    WaitTime = timer:now_diff(T2, T1) div 1000,
    CallerWaitTime = timer:now_diff(T2, RequestTime) div 1000,
    Log = #log{qid=Id, rwait=WaitTime, caller=Caller, cwait=CallerWaitTime},
    log(Log);

log([#rxq_req{}=H|T], Caller, RequestTime) ->
    log(H, Caller, RequestTime),
    log(T, Caller, RequestTime).


log(#log{qid=Id, rwait=Rwt, caller=Caller, cwait=Cwt}) ->

    Tstmp0 = calendar:now_to_local_time(now()),
    Tstmp = httpd_util:rfc1123_date(Tstmp0),

    Log=lists:flatten(io_lib:format("~s|~p|~.2f|~2.f|~p", [Tstmp,Id,Rwt,Cwt,Caller])),
    ok = log4erl:log(?LOGGER, debug, "~s", [Log]).
