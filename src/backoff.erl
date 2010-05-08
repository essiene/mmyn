-module(backoff).
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,
        handle_info/2,terminate/2,code_change/3]).

-export([start_link/0, status/0, stop/0]).

-record(st, {ets}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

status() ->
    gen_server:call(?MODULE, status).

stop() ->
    gen_server:call(?MODULE, stop).


init([]) ->
    error_logger:info_msg("~p starting~n", [?MODULE]),
    {ok, #st{ets=ets:new(?MODULE, [set, private])}}.

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
