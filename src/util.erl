-module(util).
-export([soap_request/5, sms_response/2, smsc_params/0, sms_format_msg/2, notify_params/0]).

-include("simreg.hrl").

-define(SMS_MSG_MAX_LEN, 156).


soap_request(Url, RqHdrs, RqBody, RsFun, Op) ->
    try ibrowse:send_req(Url, RqHdrs, post, RqBody) of
        {ok, "200", _, RsBody} -> 
            S = RsFun(RsBody),
            S#soap_response{op=Op};
        {ok, Status, _, _} -> 
            error_logger:error_msg("[~p] Got HTTP ~p while calling ~p~n", [self(), Status, Url]),
            Msg = "HTTP " ++ Status,
            #soap_response{status=Status, message=Msg, op=Op};
        {error, Reason}=Error ->
            error_logger:error_msg("[~p] Got ~p while calling ~p~n", [self(), Error, Url]),
            Msg = sms_format_msg("error: ~p", [Reason]),
            #soap_response{status=1000, message=Msg, op=Op}
    catch 
        Type:Message ->
            error_logger:error_msg("[~p] Got ~p while calling ~p~n", [self(), {Type, Message}, Url]),
            Msg = sms_format_msg("~p : ~p", [Type, Message]),
            #soap_response{status=1000, message=Msg, op=Op}
    end.

smsc_params() -> 
    {ok, Host} = application:get_env(smsc_host), 
    {ok, Port} = application:get_env(smsc_port), 
    {ok, SystemId} = application:get_env(smsc_username), 
    {ok, Password} = application:get_env(smsc_password), 
    {Host, Port, SystemId, Password}.

notify_params() -> 
    {ok, Msisdns} = application:get_env(notify_msisdns), 
    {ok, Sender} = application:get_env(notify_sender), 
    {Msisdns, Sender}.


sms_format_msg(Fmt, Args) ->
    S = lists:flatten(io_lib:format(Fmt, Args)),
    case length(S) of
        0 ->
            "Empty Msg";
        N when N < 130 -> 
            S;
        _ ->
            S0 = string:substr(S, 1, ?SMS_MSG_MAX_LEN),
            S0 ++ "..."
    end.
