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
            Req, fun parse/1, puk).

parse(Xml) when is_list(Xml) ->
    {ok, Response, _Tail} = erlsom:parse_sax(list_to_binary(Xml), #soap_response{}, fun process/2),
    Response.



process(endDocument, Res) ->
    Res#soap_response{flag=undefined};
process({characters, X}, #soap_response{flag='STATUS'}=Res) ->
    Rc = list_to_integer(X),
    Res#soap_response{status=Rc, flag=undefined};
process({characters, X}, #soap_response{flag='ERRMSG'}=Res) ->
    Res#soap_response{message=X, flag=undefined};
process({characters, "SIM Card"}, #soap_response{flag='EQUIPMENT'}=Res) ->
    Res#soap_response{flag='READ_PUK'};
process({characters, X}, #soap_response{flag='PUK'}=Res) ->
    % TODO: are we in danger of string manipulation/ code injection here?
    {ok, Fmt} = application:get_env(msg_puk_get),
    Msg = lists:flatten(io_lib:format(Fmt, [X])),
    Res#soap_response{message=Msg, flag=undefined};
process({startElement, _, "returnCode", _, _}, Res) ->
    Res#soap_response{flag='STATUS'};
process({startElement, _, "codeDescription", _, _}, Res) ->
    Res#soap_response{flag='ERRMSG'};
process({startElement, _, "EQUIPMENT_TYPE_NAME", _, _}, Res) ->
    Res#soap_response{flag='EQUIPMENT'};
process({startElement, _, "PUK", _, _}, #soap_response{flag='READ_PUK'}=Res) ->
    Res#soap_response{flag='PUK'};
process(_, Accm) ->
    Accm.
