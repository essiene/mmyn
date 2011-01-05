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
-record(async_req, {sender, window_sz}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push(#rxq_req{}=Item) ->
    gen_server:call(?MODULE, {push, Item}).

pop() ->
    gen_server:call(?MODULE, pop).

async_pop(WindowSize) ->
    gen_server:cast(?MODULE, {async_pop_req, WindowSize, self()}).

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
                case log4erl:add_file_appender(?LOGGER, file_logger_qlog, {LogDir, LogFile, {size, LogSize}, NumRotations, Suffix, all, "%l%n"}) of
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

handle_cast({async_pop_req, W, S}, #st{async_rx=AsyncRx0}=St) ->
    Rq = #async_req{sender=S, window_sz=W},
    AsyncRx1 = queue:in(Rq, AsyncRx0),
    erlang:send_after(500, self(), async_pop),
    {noreply, St#st{async_rx=AsyncRx1}};

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
    case Spq:len() of
        0 ->
            {noitem, AsyncQ};
        _ ->
            case queue:out(AsyncQ) of
                {empty, AsyncQ} ->
                    {noreq, AsyncQ};
                {{value, #async_req{sender=S, window_sz=W}}, AsyncQ1} ->
                    case is_process_alive(S) of
                        false ->
                            handle_async_pop(AsyncQ1, Spq);
                        true ->
                            Items = spq:pop(W),
                            S ! {self(), rxq_data, Items},
                            {ok, AsyncQ1}
                    end
            end
    end.

