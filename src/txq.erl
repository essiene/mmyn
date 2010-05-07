-module(txq).
-behaviour(gen_server).
-export([init/1,handle_call/3,
        handle_cast/2,handle_info/2,
        terminate/2,code_change/3]).

-record(st, {q}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push(Item) ->
    gen_server:call(?MODULE, {push, Item}).

pop() ->
    gen_server:call(?MODULE, pop).
