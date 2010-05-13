-module(reg).
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

    {ok, Url} = application:get_env(soap_url_reg),
    util:soap_request(Url, [
            {"Accept-Encoding", "identity"},
            {"Soapaction", ""},
            {"User-Agent", "Mmayen/1.0"},
            {"Authorization", Auth}, 
            {"Content-Type", "text/xml"}],
            Req2, fun parse/1, reg).

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
process({startElement, _, "ErrorCode", _, _}, Res) ->
    Res#soap_response{flag='STATUS'};
process({startElement, _, "ErrorMessage", _, _}, Res) ->
    Res#soap_response{flag='ERRMSG'};
process(_, Accm) ->
    Accm.
