-module(util).
-export([soap_request/4, sms_response/2]).

-include("simreg.hrl").

-define(SMS_SRC, "SimReg").
-define(SMS_ERR_SRC, "SErr").
-define(NOTIFY_MSISDN, "2347062022125").
-define(MSG_SVC_UNAVAIL, "This service is temporarily unavailable. Please try again later").

soap_request(Url, RqHdrs, RqBody, RsFun) ->
    try ibrowse:send_req(Url, RqHdrs, post, RqBody) of
        {ok, "200", _, RsBody} -> 
            RsFun(RsBody);
        {ok, Status, _, _} -> 
            Msg = "HTTP " ++ Status,
            #soap_response{status=Status, message=Msg}
    catch 
        Type:Message ->
            Msg = lists:flatten(iolib:format("~p : ~p", [Type, Message])),
            #soap_response{status=1000, message=Msg}
    end.

sms_response(Dest, #soap_response{message=undefined}) ->
    simreg_tx:sendsms(?SMS_SRC, Dest, ?MSG_SVC_UNAVAIL),
    simreg_tx:sendsms(?SMS_ERR_SRC, ?NOTIFY_MSISDN, "Unable to get response");

sms_response(Dest, #soap_response{status=0, message=Msg}) ->
    simreg_tx:sendsms(?SMS_SRC, Dest, Msg);

sms_response(Dest, #soap_response{status=_N, message=Msg}) ->
    simreg_tx:sendsms(?SMS_SRC, Dest, ?MSG_SVC_UNAVAIL),
    simreg_tx:sendsms(?SMS_ERR_SRC, ?NOTIFY_MSISDN, Msg);

sms_response(Dest, Msg) ->
    simreg_tx:sendsms(?SMS_SRC, Dest, Msg).
    
