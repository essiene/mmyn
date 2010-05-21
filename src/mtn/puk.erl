-module(puk).
-export([get/2]).

-include("simreg.hrl").

get(Tid, Msisdn) ->
    Auth = string:concat("Basic ", base64:encode_to_string("eaitest:1eaitest")),
    Req = "<soapenv:Envelope 
                xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" 
                xmlns:ws=\"http://mtnn/eai/sv/ws\" 
                xmlns:get=\"http://eai.mtn.ng/sv/getprodinstdetails\"> 
                    <soapenv:Header> 
                        <ws:Header>
                            <get:Header>
                                <get:TransactionID>"
                                    ++ Tid ++ 
                                "</get:TransactionID>
                                <get:ServiceInterface>SingleView</get:ServiceInterface>
                                <get:ServiceOperation>GetProdInstDetails</get:ServiceOperation>
                            </get:Header>
                        </ws:Header> 
                   </soapenv:Header> 
                   <soapenv:Body> 
                        <ws:clientRequest>
                            <get:GetProdInstDetailsRequest> 
                                <get:MSISDN>"
                                    ++ Msisdn ++
                                "</get:MSISDN> 
                            </get:GetProdInstDetailsRequest> 
                        </ws:clientRequest> 
                    </soapenv:Body> 
                </soapenv:Envelope>",

    {ok, Url} = application:get_env(soap_url_puk),
    util:soap_request(Url, [
            {"Accept-Encoding", "identity"},
            {"Soapaction", ""},
            {"User-Agent", "Mmayen/1.0"},
            {"Authorization", Auth}, 
            {"Content-Type", "text/xml"}],
            Req, fun parse_get_response/1, puk).

parse_get_response(Xml) when is_list(Xml) ->
    {ok, Response, _Tail} = erlsom:parse_sax(list_to_binary(Xml), #soap_response{}, fun process_get_response/2),
    Response.



process_get_response(endDocument, Res) ->
    Res#soap_response{flag=undefined};
process_get_response({characters, X}, #soap_response{flag='STATUS'}=Res) ->
    Rc = list_to_integer(X),
    Res#soap_response{status=Rc, flag=undefined};
process_get_response({characters, X}, #soap_response{flag='ERRMSG'}=Res) ->
    Res#soap_response{message=X, flag=undefined};
process_get_response({characters, "SIM Card"}, #soap_response{flag='EQUIPMENT'}=Res) ->
    Res#soap_response{flag='READ_PUK'};
process_get_response({characters, X}, #soap_response{flag='PUK'}=Res) ->
    % TODO: are we in danger of string manipulation/ code injection here?
    {ok, Fmt} = application:get_env(msg_puk_get),
    Msg = lists:flatten(io_lib:format(Fmt, [X])),
    Res#soap_response{message=Msg, flag=undefined};
process_get_response({startElement, _, "returnCode", _, _}, Res) ->
    Res#soap_response{flag='STATUS'};
process_get_response({startElement, _, "codeDescription", _, _}, Res) ->
    Res#soap_response{flag='ERRMSG'};
process_get_response({startElement, _, "EQUIPMENT_TYPE_NAME", _, _}, Res) ->
    Res#soap_response{flag='EQUIPMENT'};
process_get_response({startElement, _, "PUK", _, _}, #soap_response{flag='READ_PUK'}=Res) ->
    Res#soap_response{flag='PUK'};
process_get_response(_, Accm) ->
    Accm.
