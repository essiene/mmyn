%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(mmyn).
-include("mmyn.hrl").

-author('author <author@example.com>').

-behaviour(application).
-behaviour(supervisor).


-export([start/0, start/2, start_link/0, 
        status/0, stop/0, stop/1]).

-export([init/1]).

%% @spec start() -> ok
%% @doc Start the mmyn server.
start() ->
    application:start(mmyn, temporary).

start(_Type, _StartArgs) ->
   start_link(). 

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

status() ->
    mmyn_manager:status().

%% @spec stop() -> ok
%% @doc Stop the mmyn server.
stop() ->
    case whereis(mmyn) of
        undefined -> ok;
        Pid -> exit(Pid, shutdown)
    end,
    application:stop(mmyn).

stop(_State) ->
    ok.


init([]) ->
    Tlog = {tlog,
        {tlog, start_link, []},
        permanent, 5000, worker, [tlog]},

    BackOff = {backoff,
        {backoff, start_link, []},
        permanent, 5000, worker, [backoff]},

    TxQ = {txq, 
        {txq, start_link, []},
        permanent, 5000, worker, [txq]},

    TxSup = {tx_sup,
        {tx_sup, start_link, []},
        permanent, infinity, supervisor, [tx_sup]},

    TxNanny = {tx_nanny,
        {nanny, start_link, [tx_nanny, {tx_nanny_num_children, tx_nanny_backoff}, 
                                        {tx_sup,start_child}, 
                                        {esmetx,stop},{undefined, undefined}]},
        permanent, 5000, worker, [tx_nanny]},

    Notify = {notify, 
        {notify, start_link, []},
        permanent, 5000, worker, [notify]},

    RxQ = {rxq, 
        {rxq, start_link, []},
        permanent, 5000, worker, [rxq]},

    RTable  = {rtable,
        {rtable, start_link, []},
        permanent, 5000, worker, [rtable]},

    RxSup = {rx_sup,
        {rx_sup, start_link, []},
        permanent, infinity, supervisor, [rx_sup]},

    RxNanny = {rx_nanny,
        {nanny, start_link, [rx_nanny, {rx_nanny_num_children, rx_nanny_backoff}, 
                                            {rx_sup,start_child}, 
                                            {esmerx,stop}, {undefined, undefined}]},
        permanent, 5000, worker, [rx_nanny]},

    RxWkrSup = {rxwkr_sup,
        {rxwkr_sup, start_link, []},
        permanent, infinity, supervisor, [rxwkr_sup]},

    RxWkrNanny = {rxwkr_nanny,
        {nanny, start_link, [rxwkr_nanny, {rxwkr_nanny_num_children, rxwkr_nanny_backoff}, 
                                            {rxwkr_sup,start_child}, 
                                            {rxworker,stop}, {undefined, undefined}]},
        permanent, 5000, worker, [rxwkr_nanny]},

    SoapServer = {mmyn_soapsrv,
        {mmyn_soapsrv, start_link, []},
        permanent, 5000, worker, [mmyn_soapsrv]},

    WebServer = {mmyn_misultin,
        {mmyn_misultin, start_link, []},
        permanent, 5000, worker, [mmyn_misultin]},

    Processes = [Tlog, BackOff, TxQ, TxSup, TxNanny, 
        Notify, RxQ, RTable, RxSup, RxNanny, RxWkrSup, RxWkrNanny, 
        SoapServer, WebServer],

    {ok, {{one_for_one, 10, 10}, Processes}}.
