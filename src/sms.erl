    -module(sms).
-include("mmyn.hrl").

-export([send/3, send/4]).

-record(req, {status=55, sender, msisdn, message, flag}).



send(Src, Xml, Module) ->
    try parse(Xml) of
        #req{status=0, sender=undefined, msisdn=Msisdn, message=Message} ->
            {Status, Detail} = send(Src, Msisdn, Message, Module),
            #soap_response{status=Status, message=Detail};
        #req{status=0, sender=Sender, msisdn=Msisdn, message=Message} ->
            {Status, Detail} = send(Sender, Msisdn, Message, Module),
            #soap_response{status=Status, message=Detail};
        #req{status=N, msisdn=undefined} ->
            #soap_response{status=N, message="MSISDN unspecified"};
        #req{status=N, message=undefined} ->
            #soap_response{status=N, message="Message unspecified"};
        #req{status=N} ->
            #soap_response{status=N, message="Error parsing request"}
    catch
        _Any:Message ->
            #soap_response{status=505, message=Message}
    end.

send(Src, Dst, Msg, Module) ->
    txq:push(#txq_req{src=Src, dst=Dst, message=Msg, module=Module}),
    nanny:wake_all(tx_nanny),
    {0, "Accepted for delivery"}.


parse(Xml) ->
    {ok, Response, _Tail} = erlsom:parse_sax(Xml, #req{}, fun process/2),
    Response.



process(endDocument, Req) ->
    Req#req{flag=undefined};
process({characters, X}, #req{flag='SENDER'}=Req) ->
    Req#req{sender=X, flag=undefined};
process({characters, X}, #req{flag='MSISDN'}=Req) ->
    Req#req{msisdn=X, flag=undefined};
process({characters, X}, #req{flag='MESSAGE'}=Req) ->
    Req#req{status=0, message=X, flag=undefined};
process({startElement, _, "sender", "sms", _}, Req) ->
    Req#req{flag='SENDER'};
process({startElement, _, "msisdn", "sms", _}, Req) ->
    Req#req{flag='MSISDN'};
process({startElement, _, "message", "sms", _}, Req) ->
    Req#req{flag='MESSAGE'};
process(_, Accm) ->
    Accm.
