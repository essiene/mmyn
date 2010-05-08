-module(nanny).
-behaviour(gen_fsm).
-export([init/1,handle_sync_event/4,
        handle_event/3,handle_info/3,
        terminate/3,code_change/4]).

-export([idle/2, active_rx/2]).
-export([active_rx/3]).

-export([start_link/0, add_rx/0]).

-record(st, {rx, rx_count=0}).


start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

add_rx() ->
    gen_fsm:sync_send_event(?MODULE, add_rx).

init([]) ->
    {ok, Count} = application:get_env(rx_count),
    Tid = ets:new(nanny, [set, private]),
    tag_and_load(Tid, Count),
    gen_fsm:send_event_after(1000, babysit),
    {ok, idle, #st{rx=Tid}}.

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

active_rx(babysit, St) ->
    babysit(St).

% StateName/3

active_rx(add_rx, _From, #st{rx=Ets, rx_count=Count}=St) ->
    case start_if_not_running(Ets, Count+1) of
        ok ->
            {next_state, active_rx, St#st{rx_count=Count+1}};
        {error, _Reason} ->
            {next_state, active_rx, St}
    end.


% Privates

babysit(#st{rx=Ets}=St) ->
    Count = start_all(Ets),
    case Count of
        0 -> 
            gen_fsm:send_event_after(3000, babysit),
            {next_state, idle, St};
        N ->
            gen_fsm:send_event_after(20000, babysit),
            {next_state, active_rx, St#st{rx_count=N}}
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
    case rx_sup:start_child(Id) of
        {ok, Pid} ->
            ets:insert(Ets, {Id, Pid}),
            error_logger:info_msg("Receiver ~p started with Pid: ~p~n", [Id, Pid]),
            ok;
        {error, Reason} ->
            error_logger:info_msg("Receiver ~p failed to start with Reason: ~p~n", [Id, Reason]),
            {error, Reason}
    end.




tag_and_load(Ets, Count) ->
    L = tag(Ets, Count, undefined, []),
    ets:insert(Ets, L).

tag(_, 0, _, Accm) ->
    Accm;
tag(Ets, N, Value, Accm) ->
    tag(Ets, N-1, Value, [{N, Value}|Accm]).
