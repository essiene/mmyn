%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the simreg application.

-module(rxwkr_sup).
-author('author <author@example.com>').
-include("simreg.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/0, start_child/1, ping/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Id) ->
    supervisor:start_child(?MODULE, [Id]).

ping() ->
   Children = supervisor:which_children(?MODULE),
   ping(Children, []).

ping([], Accm) ->
    Accm;
ping([{_, Pid, _, _}|Rest], Accm) ->
    Pong = rxworker:ping(Pid),
    ping(Rest, [Pong|Accm]).


%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->

    Rx = {rxworker, 
        {rxworker, start_link, []},
        temporary, 5000, worker, [rxworker]},

    Processes = [Rx],

    {ok, {{simple_one_for_one, 10, 10}, Processes}}.
