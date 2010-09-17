-module(simreg_misultin).
-behaviour(gen_server).
-include_lib("misultin/include/misultin.hrl").
-include("simreg.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).
-export([get/2,post/2,put/2,delete/2, get_value/2]).
-export([handle_http/1]).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

init([]) ->
    {ok, Ip} = application:get_env(listen),
    {ok, Port} = application:get_env(port),
    {ok, Backlog} = application:get_env(listen_backlog),

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
    error_logger:info_msg("Dispatching: simreg_misultin:~p(~p, ...)~n", [Method3, Resource]),
    simreg_misultin:Method3(Resource, Req).


get(["sendsms"], Req) ->
    QueryString = Req:parse_qs(),

    error_logger:info_msg("QueryString: ~p~n", [QueryString]),

    case proplists:is_defined("wsdl", QueryString) of
        true -> 
            Req:ok([{"Content-Type", "text/xml"}], ?WSDL);
        false -> 
            Req:ok([{"Content-Type", "text/plain"}], "Mmayen/1.0\r\nSimReg Services/1.0\r\n")
    end;

get(["send"], Req) ->
    QueryString = Req:parse_qs(),

    error_logger:info_msg("QueryString: ~p~n", [QueryString]),

    {"to", Dst} = proplists:lookup("to", QueryString),
    {"msg", Msg} = proplists:lookup("msg", QueryString),

    ok = sms:send("SimReg", Dst, Msg, simreg_misultin),

    Req:ok([{"Content-Type", "text/plain"}], "0 : Accepted for delivery\r\n");

get(Other, Req) ->
    error_logger:info_msg("Get request: ~p ~p~n", [Other, Req]),
    Req:respond(404, "Foo Found\r\n").

post(["sendsms"], Req) ->
    #soap_response{status=Status, message=Message} = sms:send("SimReg", Req:get(body), simreg_misultin),
    Xml0 = io_lib:format(?SENDSMS_RESPONSE_TEMPLATE, [Status, Message]),
    Xml1 = lists:flatten(Xml0),
    error_logger:info_msg("Response: ~p~n", [Xml1]),
    Req:ok([{"Content-Type", "text/xml"}], Xml1);
    
post(_Path, Req) ->
    Req:respond(404, "Not Found\r\n").

put(_Path, Req) ->
    Req:respond(404, "Not Found\r\n").

delete(_Path, Req) ->
    Req:respond(404, "Not Found\r\n").


get_value(Key, QueryString) ->
    case proplists:get_value(Key, QueryString) of
        undefined ->
            throw({required_parameter_missing, Key});
        Value ->
            Value
    end.
