-module(webservice).
-export([
       get/2,
       post/2,
       delete/2,
       put/2
    ]).

-export([
        get_value/2
    ]).

-include_lib("misultin/include/misultin.hrl").
-include("simreg.hrl").

get(["sendsms"], Req) ->
    QueryString = Req:parse_qs(),

    error_logger:info_msg("QueryString: ~p~n", [QueryString]),

    case proplists:is_defined("wsdl", QueryString) of
        true -> 
            Req:ok([{"Content-Type", "text/xml"}], ?WSDL);
        false -> 
            Req:ok([{"Content-Type", "text/plain"}], "Mmayen/1.0\r\nSimReg Services/1.0\r\n")
    end;

get(Other, Req) ->
    error_logger:info_msg("Get request: ~p ~p~n", [Other, Req]),
    Req:respond(404, "Foo Found\r\n").

post(["sendsms"], Req) ->
    #soap_response{status=Status, message=Message} = sms:send("SimReg", Req:get(body)),
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
