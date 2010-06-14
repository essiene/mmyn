-module(nanny).
-behaviour(gen_fsm).
-export([init/1,handle_sync_event/4,
        handle_event/3,handle_info/3,
        terminate/3,code_change/4]).

-export([idle/2, active/2]).
-export([active/3]).

-export([start_link/5, add_child/1, babysit_all/1, wake_all/1]).

-record(st, {ets, count=0, backoff, id, start_mf, stop_mf, wake_mf, last_wake, wake_threshold}).


start_link(Id, Env, StartMf, StopMf, WakeMf) ->
    gen_fsm:start_link({local, Id}, ?MODULE, [Id, Env, StartMf, StopMf, WakeMf], []).

add_child(Id) ->
    gen_fsm:sync_send_event(Id, add_child).

babysit_all(Id) ->
    gen_fsm:send_event(Id, babysit).

wake_all(Id) ->
    gen_fsm:send_event(Id, wake).

init([Id, {EnvChildren, EnvBackoff}, StartMf, StopMf, {WakeMf, WakeThreshold}]) ->
    process_flag(trap_exit, true),

    {ok, Count} = application:get_env(EnvChildren),
    {ok, {Min, Max, Delta}} = application:get_env(EnvBackoff),

    Tid = ets:new(Id, [set, private]),
    tag_and_load(Tid, Count),
	BackoffMfa = {?MODULE, babysit_all, [Id]},
    case backoff:register(Min, Max, Delta, BackoffMfa) of
        ok -> 
            {ok, idle, #st{ets=Tid, backoff={Min, Max, Delta, BackoffMfa}, id=Id, start_mf=StartMf,
                              stop_mf=StopMf, wake_mf=WakeMf,
                              wake_threshold=WakeThreshold}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_sync_event(R, _F, StName, St) ->
    {reply, {error, {illegal_request, R}}, StName, St}.


handle_event(_R, StName, St) ->
    {next_state, StName, St}.


handle_info(_R, StName, St) ->
    {next_state, StName, St}.


terminate(_R, _StName, #st{id=Id}=St) ->
    error_logger:info_msg("~p is going down~n", [Id]),
    backoff:deregister(),
    stop(St),
    ok.


code_change(_OldVsn, StName, St, _Extra) ->
    {ok, StName, St}.


% StateName/2

idle(babysit, St) ->
    babysit(St);

idle(_, St) ->
    {next_state, idle, St}.

active(babysit, St) ->
    babysit(St);

active(wake, #st{wake_threshold=undefined}=St) ->
    wake(St),
    T = calendar:local_time(),
    {next_state, active, St#st{last_wake=T}};

active(wake, #st{last_wake=undefined}=St) ->
    wake(St),
    T = calendar:local_time(),
    {next_state, active, St#st{last_wake=T}};

active(wake, #st{last_wake=T1, wake_threshold=Threshold}=St) ->
    T2 = calendar:local_time(),
    Tdiff = calendar:time_difference(T1, T2),
    MilliSecs = daystime_to_millisecs(Tdiff),

    case MilliSecs of
        N when N < Threshold ->
            {next_state, active, St};
        _ -> 
            active(wake, St#st{last_wake=undefined})
    end;

active(_, St) ->
    {next_state, active, St}.

% StateName/3

active(add_child, _From, #st{ets=Ets, count=Count}=St) ->
    case start_if_not_running(Ets, Count+1) of
        ok ->
            {reply, ok, active, St#st{count=Count+1}};
        {error, Reason} ->
            {reply, {error, Reason}, active, St}
    end;

active(R, _F, St) ->
    {reply, {error, R}, active, St}.


% Privates

babysit(#st{backoff={Min,Max,Delta,Mfa}}=St) ->
    Count = start_all(St),
    case Count of
        0 -> 
            ok = backoff:regular(Min,Max,Delta,Mfa),
            {next_state, idle, St};
        N ->
            ok = backoff:increment(Min,Max,Delta,Mfa),
            {next_state, active, St#st{count=N}}
    end.

wake(#st{ets=Ets}=St) ->
    Id = ets:first(Ets),
    wake(St, Id).

wake(_, '$end_of_table') ->
    ok;
wake(#st{wake_mf=undefined}, _) ->
    ok;
wake(#st{ets=Ets, wake_mf={Module, Func}}=St, Id0) ->
    case ets:lookup(Ets, Id0) of
        [] ->
            ok;
        [{Id0, undefined}] ->
            ok;
        [{Id0, Pid}] when is_pid(Pid) ->
            case is_process_alive(Pid) of
                false -> 
                    ok;
                true -> 
                    Module:Func(Pid)
            end
    end,
    Id = ets:next(Ets, Id0),
    wake(St, Id).

stop(#st{ets=Ets}=St) ->
    Id = ets:first(Ets),
    stop(St, Id).

stop(_, '$end_of_table') ->
    ok;
stop(#st{stop_mf=undefined}, _) ->
    ok;
stop(#st{ets=Ets, stop_mf={Module,Func}}=St, Id0) ->
    case ets:lookup(Ets, Id0) of
        [] ->
            ok;
        [{Id0, undefined}] ->
            ok;
        [{Id0, Pid}] when is_pid(Pid) ->
            case is_process_alive(Pid) of
                false -> 
                    ok;
                true -> 
                    Module:Func(Pid)
            end
    end,
    Id = ets:next(Ets, Id0),
    stop(St, Id).


start_all(#st{ets=Ets}=St) ->
    Id = ets:first(Ets),
    start_next(St, Id, 0).

start_next(_, '$end_of_table', Count) ->
    Count;
start_next(#st{ets=Ets}=St, Id0, C0) ->
    Id = ets:next(Ets, Id0),

    case start_if_not_running(St, Id0) of
        ok ->
            start_next(St, Id, C0+1);
        {error, _Reason} ->
            start_next(St, Id, C0)
    end.

start_if_not_running(#st{ets=Ets}=St, Id) ->
    case ets:lookup(Ets, Id) of
        [] ->
            start_child(St, Id);
        [{Id, undefined}] ->
            start_child(St, Id);
        [{Id, Pid}] when is_pid(Pid) ->
            case is_process_alive(Pid) of
                false -> 
                    start_child(St, Id);
                true -> 
                    ok  
            end
    end.
            
start_child(#st{ets=Ets, start_mf={Module, Func}, id=Id}, Cid) ->
    case Module:Func(Cid) of
        {ok, Pid} ->
            ets:insert(Ets, {Cid, Pid}),
            error_logger:info_msg("[~p] Child ~p started with Pid: ~p~n", [Id, Cid, Pid]),
            ok;
        {error, Reason} ->
            error_logger:info_msg("[~p] Child ~p failed to start with Reason:
                                 ~p~n", [Id, Cid, Reason]),
            {error, Reason}
    end.




tag_and_load(Ets, Count) ->
    L = tag(Ets, Count, undefined, []),
    ets:insert(Ets, L).

tag(_, 0, _, Accm) ->
    Accm;
tag(Ets, N, Value, Accm) ->
    tag(Ets, N-1, Value, [{N, Value}|Accm]).

daystime_to_millisecs({Days, {H, M, S}}) ->
    Ms1 = 864000 * Days,
    Ms2 = Ms1 + 3600 * H,
    Ms3 = Ms2 + 60 * M,
    Ms4 = Ms3 + S,
    Ms4 * 1000.
