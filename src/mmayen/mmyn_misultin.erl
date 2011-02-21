-module(mmyn_misultin).
-behaviour(gen_server).
-include_lib("misultin/include/misultin.hrl").
-include("mmyn.hrl").

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
    mmyn_soapsrv:setup(mmyn, {mmyn_soap, handler}, "var/www/mmyn-2.0.wsdl", "mmyn"),
    mmyn_soapsrv:setup(notify, {mmyn_soap, notify}, "var/www/notify-2.0.wsdl", "mmyn"),
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
    mmyn_misultin:Method3(Resource, Req).

get(["soap", "2.0", "notify"], Req) ->
    QueryString = Req:parse_qs(),

    case proplists:is_defined("wsdl", QueryString) of
        true -> 
            Req:file("var/www/notify-2.0.wsdl");
        false -> 
            Req:ok([{"Content-Type", "text/plain"}], 
                     "Server: Mmayen SMPP Gateway\r\nRelease: 1.0\r\nWeb Services Version: 2.0\r\nNotify Request: 2.0\r\n")
    end;

get(["soap", "2.0"], Req) ->
    QueryString = Req:parse_qs(),

    case proplists:is_defined("wsdl", QueryString) of
        true -> 
            Req:file("var/www/mmyn-2.0.wsdl");
        false -> 
            Req:ok([{"Content-Type", "text/plain"}], 
                     "Server: Mmayen SMPP Gateway\r\nRelease: 1.0\r\nWeb Services Version: 2.0\r\n")
    end;

get(["sendsms"], Req) ->
    QueryString = Req:parse_qs(),

    case proplists:is_defined("wsdl", QueryString) of
        true -> 
            Req:ok([{"Content-Type", "text/xml"}], ?WSDL);
        false -> 
            Req:ok([{"Content-Type", "text/plain"}], "Mmayen/1.0\r\nMmyn Services/1.0\r\n")
    end;

get(["send"], Req) ->
    QueryString = Req:parse_qs(),

    try deliver_msg(QueryString) of
        {Code, Message} ->
            Req:ok([{"Content-Type", "text/plain"}], lists:concat([Code,":",Message,"\r\n"]))
    catch
        throw: {required_parameter_missing, Key} ->
            ErrMsg = lists:concat(["1 : Required parameter missing - '", Key, "'"]),
            Req:respond(400, ErrMsg);
        Type : Msg ->
            error_logger:error_msg("/SEND: ~p:~p~n", [Type, Msg]),
            Req:respond(500, "Internal Error")
    end;

get(_, Req) ->
    Req:respond(404, "Foo Found\r\n").

post(["sendsms"], Req) ->
    #soap_response{status=Status, message=Message} = sms:send("mmyn", Req:get(body), mmyn_misultin),
    Xml0 = io_lib:format(?SENDSMS_RESPONSE_TEMPLATE, [Status, Message]),
    Xml1 = lists:flatten(Xml0),
    Req:ok([{"Content-Type", "text/xml"}], Xml1);

post(["soap", "2.0", "notify"], Req) ->
    dispatch_soap_req(Req, notify);
 
post(["soap", "2.0"], Req) ->
    dispatch_soap_req(Req, mmyn);
   
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

deliver_msg(QueryString) ->
    Src = get_value("from", QueryString),
    Dst = get_value("to", QueryString),
    Msg = get_value("msg", QueryString),
    sms:send(Src, Dst, Msg, mmyn_misultin).

dispatch_soap_req(Req, EndpointName) ->
    Headers = Req:get(headers),
    Body = Req:get(body),
    try get_value("Soapaction", Headers) of
        SoapAction ->
            Stripped = string:strip(SoapAction, both, $"), % soapUI sends  requotes the Soapaction header like so: "\"soapaction\""
            {_, Xml, ResCode} = mmyn_soapsrv:dispatch(EndpointName, Body, Stripped),
            Req:respond(ResCode, [{"Content-Type", "text/xml"}], Xml)
    catch
        throw: {required_parameter_missing, "Soapaction"} ->
            ErrMsg = "Header Missing - SOAPAction",
            Req:respond(400, ErrMsg);
        Type : Msg ->
            error_logger:error_msg("SOAP: ~p:~p~n", [Type, Msg]),
            Req:respond(500, "Internal Error")
    end.
 
