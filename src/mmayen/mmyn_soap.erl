-module(mmyn_soap).
-include("mmyn.hrl").
-include("mmyn_soap.hrl").
-include("notify_soap.hrl").
-export([handler/3, notify/3]).


handler("sendsms", _, [#'mmyn:SendSms'{fields=Request}]) ->
    #'mmyn:SendSmsRequest'{sender=Sender, msisdn=Msisdn, message=Message, size=Size}=Request,

    {Status, Detail} = case Size of 
        N when N < 1 ->
            {410, "Recipient list size less than 1"}; 
        N when N > 128 ->
            {411, "Recipient list size greater than maximum of 128"};
        N when N =< 128 ->
            sms:send(Sender, {Size, Msisdn}, Message, soap_sendsms)
    end,


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
    {error, lists:concat(["SOAPAction not supported: ", SoapAction])}.

notify("notify", _, [#'mmyn:Notify'{fields=Request}]) ->
    #'mmyn:NotifyRequest'{id=_Id, shortcode=_Sc, keyword=_Kw, 
        msisdn=_Msisdn, message=_Message, 'max-ttl'=Ttl} = Request,

    Response = #'mmyn:Response'{
        fields=#'mmyn:NotifyResponse' {
            ttl=Ttl div 5,
            'wait-for-reply' = false,
            status = 0,
            detail = "Accepted for processing"
        }
    },

    {ok, Response};

notify(SoapAction, _, _) ->
    {error, lists:concat(["SOAPAction not supported: ", SoapAction])}.

