%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the simreg application.

-module(tx_sup).
-author('author <author@example.com>').
-include("simreg.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0, start_child/1]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

start_child(Id) ->
    supervisor:start_child(?MODULE, [Id]).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->

    Tx = {tx, 
        {tx, start_link, []},
        transient, 5000, worker, [tx]},

    Processes = [Tx],

    {ok, {{simple_one_for_one, 10, 10}, Processes}}.
