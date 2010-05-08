-module(tx_nanny).
-behaviour(gen_fsm).
-export([init/1,handle_sync_event/4,
        handle_event/3,handle_info/3,
        terminate/3,code_change/4]).

-export([idle/2, active/2]).
-export([active/3]).

-export([start_link/0, add_child/0]).

-record(st, {ets, count=0}).


start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

add_child() ->
    gen_fsm:sync_send_event(?MODULE, add_child).

init([]) ->
    {ok, Count} = application:get_env(transmitters),
    Tid = ets:new(nanny, [set, private]),
    tag_and_load(Tid, Count),
    gen_fsm:send_event_after(1000, babysit),
    {ok, idle, #st{ets=Tid}}.

handle_sync_event(R, _F, StName, St) ->
    {reply, {error, {illegal_request, R}}, StName, St}.


handle_event(_R, StName, St) ->
    {next_state, StName, St}.


handle_info(_R, StName, St) ->
    {next_state, StName, St}.


terminate(_R, _StName, _St) ->
    ok.


code_change(_OldVsn, StName, St, _Extra) ->
    {ok, StName, St}.


% StateName/2

idle(babysit, St) ->
    babysit(St).

active(babysit, St) ->
    babysit(St).

% StateName/3

active(add_child, _From, #st{ets=Ets, count=Count}=St) ->
    case start_if_not_running(Ets, Count+1) of
        ok ->
            {reply, ok, active, St#st{count=Count+1}};
        {error, Reason} ->
            {reply, {error, Reason}, active, St}
    end.


% Privates

babysit(#st{ets=Ets}=St) ->
    Count = start_all(Ets),
    case Count of
        0 -> 
            gen_fsm:send_event_after(3000, babysit),
            {next_state, idle, St};
        N ->
            gen_fsm:send_event_after(20000, babysit),
            {next_state, active, St#st{count=N}}
    end.


start_all(Ets) ->
    Id = ets:first(Ets),
    start_next(Ets, Id, 0).

start_next(_, '$end_of_table', Count) ->
    Count;
start_next(Ets, Id0, C0) ->
    Id = ets:next(Ets, Id0),

    case start_if_not_running(Ets, Id0) of
        ok ->
            start_next(Ets, Id, C0+1);
        {error, _Reason} ->
            start_next(Ets, Id, C0)
    end.

start_if_not_running(Ets, Id) ->
    case ets:lookup(Ets, Id) of
        [] ->
            start_child(Ets, Id);
        [{Id, undefined}] ->
            start_child(Ets, Id);
        [{Id, Pid}] when is_pid(Pid) ->
            case is_process_alive(Pid) of
                false -> 
                    start_child(Ets, Id);
                true -> 
                    ok  
            end
    end.
            
start_child(Ets, Id) ->
    case tx_sup:start_child(Id) of
        {ok, Pid} ->
            ets:insert(Ets, {Id, Pid}),
            error_logger:info_msg("Transmitter ~p started with Pid: ~p~n", [Id, Pid]),
            ok;
        {error, Reason} ->
            error_logger:info_msg("Transmitter ~p failed to start with Reason: ~p~n", [Id, Reason]),
            {error, Reason}
    end.




tag_and_load(Ets, Count) ->
    L = tag(Ets, Count, undefined, []),
    ets:insert(Ets, L).

tag(_, 0, _, Accm) ->
    Accm;
tag(Ets, N, Value, Accm) ->
    tag(Ets, N-1, Value, [{N, Value}|Accm]).
