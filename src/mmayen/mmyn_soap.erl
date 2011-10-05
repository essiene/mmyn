-module(mmyn_soap).
-include("mmyn.hrl").
-include("mmyn_soap.hrl").
-include("notify_soap.hrl").
-export([handler/4, notify/4]).


handler(_Header, [#'mmyn:SendSms'{fields=Request}], "sendsms", _SessionValue) ->
    #'mmyn:SendSmsRequest'{sender=Sender, msisdn=Msisdn, message=Message, size=Size}=Request,

    {Status, Detail} = case Size of 
        % this must be the first clause as
        % in erlang 'undefined' is greater than 1
        % or rather atoms are greater than numbers
        undefined ->
            % set size to 1 when not sent by client
            sms:send(Sender, {1, Msisdn}, Message, soap_sendsms);
        N when N < 1 ->
            {410, "Recipient list size less than 1"}; 
        N when N > 128 ->
            {411, "Recipient list size greater than maximum of 128"};
        _ ->
            sms:send(Sender, {Size, Msisdn}, Message, soap_sendsms)
    end,


    Response = #'mmyn:SendSmsResponse'{
        fields=#'mmyn:MmynResponse'{
            status=Status, 
            detail=Detail
        }
    },
    [Response];


handler(_Header, [#'mmyn:Reply'{fields=Request}], "reply", _SessionValue) ->
    #'mmyn:ReplyRequest'{id=_Id, sender=Sender, msisdn=Msisdn, message=Message}=Request,

    {Status, Detail} = sms:send(Sender, Msisdn, Message, soap_reply),

    Response = #'mmyn:ReplyResponse'{
        fields=#'mmyn:MmynResponse'{
            status=Status, 
            detail=Detail
        }
    },
    [Response];

handler(_Header, _Body, SoapAction, _) ->
    {error, lists:concat(["SOAPAction not supported: ", SoapAction])}.

notify(_Header, [#'mmyn:Notify'{fields=Request}], "notify", _SessionValue) ->
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

    [Response];

notify(_Header, _Body, SoapAction, _SessionValue) ->
    {error, lists:concat(["SOAPAction not supported: ", SoapAction])}.
