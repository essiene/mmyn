-module(txq).
-behaviour(gen_server).
-export([init/1,handle_call/3,
        handle_cast/2,handle_info/2,
        terminate/2,code_change/3]).

-export([start_link/0, push/1, pop/0]).

-record(st, {q}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push(Item) ->
    gen_server:call(?MODULE, {push, Item}).

pop() ->
    gen_server:call(?MODULE, pop).


init([]) ->
    {ok, #st{q=queue:new()}}.

handle_call({push, Item}, _F, #st{q=Q}=St) ->
    Q1 = queue:in(Item, Q),
    {reply, ok, St#st{q=Q1}};

handle_call(pop, _F, #st{q=Q}=St) ->
    case queue:out(Q) of
        {empty, Q} -> 
            {reply, {ok, '$empty'}, St};
        {{value, V}, Q1} ->
            {reply, {ok, V}, St#st{q=Q1}}
    end;

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
