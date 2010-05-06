-module(simreg_misultin).
-behaviour(gen_server).
-include_lib("misultin/include/misultin.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/3, stop/0]).
-export([handle_http/1]).


start_link(Ip, Port, Backlog) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Ip, Port, Backlog], []).

stop() ->
	gen_server:cast(?MODULE, stop).

init([Ip, Port, Backlog]) ->
	process_flag(trap_exit, true),
	misultin:start_link([{loop, fun handle_http/1}, {ip, Ip}, {port, Port}, {backlog, Backlog}]),
	erlang:monitor(process, misultin),
    error_logger:info_msg("Webservice started~n"),
	{ok, nil}.

handle_call(_Request, _From, State) ->
	{reply, undefined, State}.

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

% handle info when misultin server goes down -> take down misultin_gen_server too [the supervisor will take everything up again]
handle_info({'DOWN', _, _, {misultin, _}, _}, State) ->
	{stop, normal, State};

% handle_info generic fallback (ignore)
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	misultin:stop(),
	terminated.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_http(Req) -> 
    Method = Req:get(method), 
    Method1 = atom_to_list(Method),
    Method2 = string:to_lower(Method1),
    Method3 = list_to_atom(Method2),
    Resource = Req:resource([lowercase, urldecode]),
    error_logger:info_msg("Dispatching webservice. Method:Resource: ~p:~p~n", [Method3, Resource]),
    apply(webservice, Method3, [Resource, Req]).
