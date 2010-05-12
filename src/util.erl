-module(util).
-export([soap_request/5, sms_response/2, smsc_params/0]).

-include("simreg.hrl").

-define(SMS_SRC, "SimReg").
-define(SMS_ERR_SRC, "SErr").
-define(NOTIFY_MSISDN, "2347034494316").
-define(MSG_SVC_UNAVAIL, "This service is temporarily unavailable. Please try again later").

-define(FORMAT_MSG(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).

soap_request(Url, RqHdrs, RqBody, RsFun, Op) ->
    try ibrowse:send_req(Url, RqHdrs, post, RqBody) of
        {ok, "200", _, RsBody} -> 
            S = RsFun(RsBody),
            S#soap_response{op=Op};
        {ok, Status, _, _} -> 
            Msg = "HTTP " ++ Status,
            #soap_response{status=Status, message=Msg, op=Op};
        {error, Reason} ->
            Msg = ?FORMAT_MSG("error: ~p", [Reason]),
            #soap_response{status=1000, message=Msg}
    catch 
        Type:Message ->
            Msg = ?FORMAT_MSG("~p : ~p", [Type, Message]),
            #soap_response{status=1000, message=Msg, op=Op}
    end.

sms_response(Dest, #soap_response{message=undefined}) ->
    sms:send(?SMS_SRC, Dest, ?MSG_SVC_UNAVAIL),
    sms:send(?SMS_ERR_SRC, ?NOTIFY_MSISDN, "Unable to get response");

sms_response(Dest, #soap_response{status=0, message=Msg}) ->
    sms:send(?SMS_SRC, Dest, Msg);

sms_response(Dest, #soap_response{status=100, op=reg, message=Msg}) ->
    sms:send(?SMS_SRC, Dest, Msg);

sms_response(Dest, #soap_response{status=N, message=Msg}) ->
    sms:send(?SMS_SRC, Dest, ?MSG_SVC_UNAVAIL),
    Msg1 = ?FORMAT_MSG("~p~n~p", [N, Msg]),
    sms:send(?SMS_ERR_SRC, ?NOTIFY_MSISDN, Msg1);

sms_response(Dest, Msg) ->
    sms:send(?SMS_SRC, Dest, Msg).
    
smsc_params() -> 
    {ok, Host} = application:get_env(smsc_host), 
    {ok, Port} = application:get_env(smsc_port), 
    {ok, SystemId} = application:get_env(smsc_username), 
    {ok, Password} = application:get_env(smsc_password), 
    {Host, Port, SystemId, Password}.
