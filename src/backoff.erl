-module(backoff).
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,
        handle_info/2,terminate/2,code_change/3]).

-export([start_link/0, status/0, stop/0]).

-export([register/4, deregister/0]).

-record(st, {ets}).
-record(spec, {pid, max, min, dlta, mfa, tref, cur, cur_dlta}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

status() ->
    gen_server:call(?MODULE, status).

stop() ->
    gen_server:call(?MODULE, stop).

register(Min, Max, Delta, Mfa) ->
    Pid = self(),
    gen_server:call(?MODULE, {register, #spec{pid=Pid, max=Max, min=Min, dlta=Delta, mfa=Mfa}}).

deregister() ->
    Pid = self(),
    gen_server:call(?MODULE, {deregister, Pid}).

init([]) ->
    error_logger:info_msg("~p starting~n", [?MODULE]),
    {ok, #st{ets=ets:new(?MODULE, [set, private])}}.

handle_call({register, #spec{pid=Pid}=Spec0}, _F, #st{ets=Ets}=St) ->
    case backoff(Spec0) of
        {ok, Spec} -> 
            ets:insert(Ets, {Pid, Spec}), 
            {reply, ok, St};
        {error, Reason} ->
            {reply, {error, Reason}, St}
    end;

handle_call({deregister, Pid}, _F, #st{ets=Ets}=St) ->
    ets:delete(Ets, Pid),
    {reply, ok, St};

handle_call(status, _F, St) ->
    {reply, {ok, alive}, St};

handle_call(stop, _F, St) ->
    error_logger:info_msg("~p stopping~n", [?MODULE]),
    {stop, normal, ok, St};

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


backoff(#spec{cur=N, tref=undefined, mfa={M, F, A}}=S) ->
    case timer:apply_after(N, M, F, A) of
        {ok, TRef} -> 
            {ok, S#spec{tref=TRef}};
        {error, Reason} ->
            {error, Reason}
    end;

backoff(#spec{tref=TRef}=S) ->
    timer:cancel(TRef),
    backoff(S#spec{tref=undefined}).


backoff_grow(#spec{cur=C, cur_dlta=D0, max=Max}=S) ->
    D = D0*2,
    case C + D of
        N when N < Max ->
            backoff(S#spec{cur=N, cur_dlta=D});
        _ ->
            backoff(S#spec{cur=Max})
    end.

backoff_normal(#spec{min=Min, dlta=Delta}=S) ->
    backoff(S#spec{cur=Min, cur_dlta=Delta}).
