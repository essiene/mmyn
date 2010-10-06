-module(simreg_services).
-include("simreg.hrl").
-behaviour(gen_sms_handler).

-define(SMS_SRC, "SimReg").
-define(SMS_ERR_SRC, "SErr").
-define(MSG_SVC_UNAVAIL, "This service is temporarily unavailable. Please try again later").
-define(LOGGER, '__service_logger').

-export([init/0,handle_sms/6,terminate/2]).
-export([msisdn_strip/2]).

init() ->
    {ok, LogDir} = application:get_env(ext_logdir),
    {ok, LogSize} = application:get_env(ext_logsize),
    {ok, NumRotations} = application:get_env(ext_logkeep),
    LogFile = "ext",
    Suffix = "log",

    AddAppender = fun () ->
        case log4erl:add_file_appender(?LOGGER, file_logger_ext, {LogDir, LogFile, {size, LogSize}, NumRotations, Suffix, all, "%l%n"}) of
            {ok, _} ->
                {ok, nil};
            {error, {already_started, _}} ->
                {ok, nil};
            {error, Reason} ->
                {stop, Reason}
        end
    end,

    case log4erl:add_logger(?LOGGER) of
        {ok, _} ->
            AddAppender();
        {error, {already_started, _}} ->
            AddAppender();
        {error, Reason} ->
            {stop, Reason}
    end.



handle_sms(_, _, _, ["-mmyn#err1" | _], _, St) ->
    {noreply,
        {error, {test, 500, "Test generated error"}},
        St};

handle_sms(_, _, _, ["-mmyn#err2" | _], _, St) ->
    {reply,
        {"mmynerr", "Test generated error"},
        {error, {test, 500, "Test generated error"}},
        St};

handle_sms(_, _, _, ["-mmyn#tst" | _], _, St) ->
    {reply,
        {"mmyn", "Tested and working"},
        {ok, {tst, 0}},
    St};

handle_sms(_, _, "789", ["-mmyn#vsn" | _], _, St) ->
    {reply, 
        {"mmyn", "eng: Mmayen\nvsn: 1.0\nos: Solaris 10"}, 
        {ok, {vsn, 0}}, 
    St};

handle_sms(_, _, "789", ["puk" | _], _, St) ->
    {ok, Msg} = application:get_env(msg_puk),
    {reply,
        {?SMS_SRC, Msg},
        {ok, {pukset, 0}},
    St};

handle_sms(Tid, _, "789", ["reg" , Msisdn0 | _], _, St) ->
    case msisdn_strip(Msisdn0, 5) of 
        {ok, Msisdn1} -> 
            Msisdn = string:concat("234", Msisdn1), 
            get_reg_status(St, Tid, Msisdn);
        {error, Reason} ->
            {noreply, {error, {reg, "500", Reason}}, St}
    end;

handle_sms(Tid, Msisdn, "789", ["reg" | _], _, St) ->
    get_reg_status(St, Tid, Msisdn);

handle_sms(_, _, _, _, _, St) ->
    {noreply, {ok, {default, "404", "Unhandled SMS"}}, St}.

terminate(_Reason, _St) ->
    ok.

% Privates

msisdn_strip(<<"+",Rest/binary>>, MinLen) ->
    msisdn_strip(Rest, MinLen);

msisdn_strip(<<"0",Rest/binary>>, MinLen) ->
    msisdn_strip(Rest, MinLen);

msisdn_strip(Msisdn, MinLen) when is_binary(Msisdn), size(Msisdn) < MinLen ->
    check_msisdn(binary_to_list(Msisdn));

msisdn_strip(<<"234",Rest/binary>>, MinLen) ->
    msisdn_strip(Rest, MinLen);

msisdn_strip(Msisdn, MinLen) when is_list(Msisdn), is_integer(MinLen) ->
    msisdn_strip(list_to_binary(Msisdn), MinLen);

msisdn_strip(Msisdn, _) ->
    check_msisdn(binary_to_list(Msisdn)).

check_msisdn(Msisdn) ->
	case string:to_integer(Msisdn) of
		{error, Reason} ->
			{error, Reason};
		{_Number, _} ->
			{ok, Msisdn}
	end.

% If no error occurs, web service will
% return Status, but if an error occurs,
% Status will not be returned and will
% thus be undefined.
%
% As long as we have a value for status, we
% are okay.

get_reg_status(To, Tid, Msisdn) ->
    case reg:get(Tid, Msisdn) of
        #soap_response{status=3}=R ->
            log(Tid, R),
            {ok, Fmt} = application:get_env(msg_reg_get_ok),
            Msg = lists:flatten(io_lib:format(Fmt, [Msisdn])),
			% we have to set status back to 0 else sms_response will see it as an error            
			sms_response(To, R#soap_response{status=0, message=Msg}); 
        #soap_response{status=-1}=R ->
            log(Tid, R),
            {ok, Fmt} = application:get_env(msg_reg_get_fail),
            Msg = lists:flatten(io_lib:format(Fmt, [Msisdn])),
            sms_response(To, R#soap_response{status=0, message=Msg});
        #soap_response{status=1}=R ->
            log(Tid, R),
            {ok, Fmt} = application:get_env(msg_reg_get_pending),
            Msg = lists:flatten(io_lib:format(Fmt, [Msisdn])),
            sms_response(To, R#soap_response{status=0, message=Msg});
        #soap_response{status=25}=R ->
            log(Tid, R),
            {ok, Fmt} = application:get_env(msg_reg_get_pending),
            Msg = lists:flatten(io_lib:format(Fmt, [Msisdn])),
            sms_response(To, R#soap_response{status=0, message=Msg});
        R ->
            log(Tid, R),
            sms_response(To, R)
    end.

sms_response(St, #soap_response{status=N, message=undefined, op=Op}) ->
    {reply,
        {?SMS_SRC, ?MSG_SVC_UNAVAIL},
        {error, {Op, N, "Unable to get response"}},
    St};

sms_response(St, #soap_response{status=0, message=Msg, op=Op}) ->
    {reply,
        {?SMS_SRC, Msg},
        {ok, {Op, 0}},
    St};

sms_response(St, #soap_response{status=N, message=Msg, op=Op}) ->
    {reply,
        {?SMS_SRC, ?MSG_SVC_UNAVAIL},
        {error, {Op, N, Msg}},
    St}.

log(Tid, #soap_response{raw_req=RawReq, raw_res=RawRes}) ->
    log4erl:log(?LOGGER, debug, "~s|~s|~s", [Tid, RawReq, RawRes]).
