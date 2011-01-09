%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the mmyn application.

-module(tx_sup).
-author('author <author@example.com>').
-include("mmyn.hrl").

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
    Pong = gen_esme34:ping(Pid),
    ping(Rest, [Pong|Accm]).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->

    Tx = {esmetx, 
        {esmetx, start_link, []},
        temporary, 5000, worker, [esmetx]},

    Processes = [Tx],

    {ok, {{simple_one_for_one, 10, 10}, Processes}}.
