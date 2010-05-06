-module(regstatus).
-include("simreg.hrl").

-export([get/1]).



get(Msisdn) ->
    Auth = string:concat("Basic ", base64:encode_to_string("eaitest:1eaitest")),
    Req0 = "<soapenv:Envelope 
                xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" 
                xmlns:ws=\"http://mtnn/eai/cis/ws\" 
                xmlns:sys=\"http://eai.mtn.ng/cis/SystemCISExtension\"> 
                    <soapenv:Header/> 
                    <soapenv:Body> 
                        <ws:VerifySubscriber> 
                            <sys:VerifySubscriberRequest>
                                <sys:SVCNo>",
    Req1 = string:concat(Req0, Msisdn),
    
    Req2 = string:concat(Req1, "</sys:SVCNo> 
                            </sys:VerifySubscriberRequest> 
                        </ws:VerifySubscriber> 
                    </soapenv:Body> 
            </soapenv:Envelope>"),

    util:soap_request("http://10.1.230.100:5013/SystemCISExtension/SystemCISExtension", [
            {"Accept-Encoding", "identity"},
            {"Soapaction", ""},
            {"User-Agent", "Mmayen/1.0"},
            {"Authorization", Auth}, 
            {"Content-Type", "text/xml"}],
            Req2, fun parse/1).

parse(Xml) when is_list(Xml) ->
    try erlsom:parse_sax(list_to_binary(Xml), #soap_response{}, fun process/2) of
        {ok, Response, _Tail} -> 
            Response;
        _Other ->
            #soap_response{status=1001, message="Parsing Error"}
    catch
        _Any:Message ->
            #soap_response{status=1002, message=Message}
    end.



process(endDocument, Res) ->
    Res#soap_response{flag=undefined};
process({characters, X}, #soap_response{flag='STATUS'}=Res) ->
    Rc = list_to_integer(X),
    Res#soap_response{status=Rc, flag=undefined};
process({characters, X}, #soap_response{flag='ERRMSG'}=Res) ->
    Res#soap_response{message=X, flag=undefined};
process({characters, X}, #soap_response{flag='PUK'}=Res) ->
    Res#soap_response{puk=X, flag=undefined};
process({startElement, _, "ErrorCode", _, _}, Res) ->
    Res#soap_response{flag='STATUS'};
process({startElement, _, "ErrorMessage", _, _}, Res) ->
    Res#soap_response{flag='ERRMSG'};
process(_, Accm) ->
    Accm.
