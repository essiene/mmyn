-module(mmyn_soap).
-include("mmyn_soap.hrl").
-include("mmyn.hrl").
-export([handler/3, notify/3]).


handler("sendsms", _, [#'mmyn:SendSms'{fields=Request}]) ->
    #'mmyn:SendSmsRequest'{sender=Sender, msisdn=Msisdn, message=Message}=Request,

    {Status, Detail} = sms:send(Sender, Msisdn, Message, soap_sendsms),

    Response = #'mmyn:SendSmsResponse'{
        fields=#'mmyn:MmynResponse'{
            status=Status, 
            detail=Detail
        }
    },
    {ok, Response};


handler("reply", _, [#'mmyn:Reply'{fields=Request}]) ->
    #'mmyn:ReplyRequest'{id=_Id, sender=Sender, msisdn=Msisdn, message=Message}=Request,

    {Status, Detail} = sms:send(Sender, Msisdn, Message, soap_reply),

    Response = #'mmyn:ReplyResponse'{
        fields=#'mmyn:MmynResponse'{
            status=Status, 
            detail=Detail
        }
    },
    {ok, Response};

handler(SoapAction, _, _) ->
    {error, list:concat(["SOAPAction not supported: ", SoapAction])}.

notify(SoapAction, _, _) ->
    {error, list:concat(["Not yet implemented: ", SoapAction])}.
