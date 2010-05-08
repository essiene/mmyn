%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(simreg).
-include("simreg.hrl").

-author('author <author@example.com>').

-behaviour(application).
-behaviour(supervisor).


-export([start/0, start/2, start_link/0, 
        status/0, stop/0, stop/1]).

-export([init/1]).

%% @spec start() -> ok
%% @doc Start the simreg server.
start() ->
    application:start(simreg, temporary).

start(_Type, _StartArgs) ->
   start_link(). 

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

status() ->
    simreg_manager:status().

%% @spec stop() -> ok
%% @doc Stop the simreg server.
stop() ->
    case whereis(simreg) of
        undefined -> ok;
        Pid -> exit(Pid, shutdown)
    end,
    application:stop(simreg).

stop(_State) ->
    ok.


init([]) ->
    TxQ = {txq, 
        {txq, start_link, []},
        permanent, 5000, worker, [txq]},

    TxSup = {tx_sup,
        {tx_sup, start_link, []},
        permanent, infinity, supervisor, [tx_sup]},

    TxNanny = {tx_nanny,
        {tx_nanny, start_link, []},
        permanent, 5000, worker, [tx_nanny]},

    RxSup = {rx_sup,
        {rx_sup, start_link, []},
        permanent, infinity, supervisor, [rx_sup]},

    RxNanny = {rx_nanny,
        {rx_nanny, start_link, []},
        permanent, 5000, worker, [rx_nanny]},

    Webservice = {simreg_misultin,
        {simreg_misultin, start_link, ["0.0.0.0", 11581, 30]},
        permanent, 5000, worker, [simreg_misultin]},

    Processes = [TxQ, TxSup, TxNanny, RxSup, RxNanny, Webservice],

    {ok, {{one_for_one, 10, 10}, Processes}}.
