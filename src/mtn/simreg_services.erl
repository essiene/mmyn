-module(simreg_services).
-include("simreg.hrl").

-export([handle_sms/3, msisdn_strip/2]).

% TODO: This entire module should have an init/1 so it can initialize and hold
%       things like config values, etc.


msisdn_strip(<<"+",Rest/binary>>, MinLen) ->
    msisdn_strip(Rest, MinLen);

msisdn_strip(<<"0",Rest/binary>>, MinLen) ->
    msisdn_strip(Rest, MinLen);

msisdn_strip(Msisdn, MinLen) when is_binary(Msisdn), size(Msisdn) < MinLen ->
    binary_to_list(Msisdn);

msisdn_strip(<<"234",Rest/binary>>, MinLen) ->
    msisdn_strip(Rest, MinLen);

msisdn_strip(Msisdn, MinLen) when is_list(Msisdn), is_integer(MinLen) ->
    msisdn_strip(list_to_binary(Msisdn), MinLen);

msisdn_strip(Msisdn, _) ->
    binary_to_list(Msisdn).

handle_sms(Src, "789", ["-mmyn#vsn" | _Rest]) ->
    sms:send("mmyn", Src, "eng: Mmayen\nvsn: 1.0\nos: Solaris 10");

handle_sms(Src, "789", ["help" | _Rest]) ->
    util:sms_response(Src, "help) Menu\npuk) Get puk\nreg)Get status");

handle_sms(Src, "789", ["puk", _PUK | _Rest]) ->
    {ok, Msg} = application:get_env(msg_puk_put),
    util:sms_response(Src, #soap_response{status=0, message=Msg});

handle_sms(Src, "789", ["puk" | _Rest]) ->
    Res = puk:get(Src),
    util:sms_response(Src, Res);

handle_sms(Src, "789", ["reg" , Msisdn0 | _Rest]) ->
    Msisdn1 = msisdn_strip(Msisdn0, 5),
    Msisdn = string:concat("234", Msisdn1),
    get_reg_status(Src, Msisdn);

handle_sms(Src, "789", ["reg" | _Rest]) ->
    get_reg_status(Src, Src);

handle_sms(Src, Dst, WordList) ->
    error_logger:info_report([unhandled_sms, {src, Src}, {dst, Dst}, {wordlist, WordList}]).

get_reg_status(To, Msisdn) ->
    case reg:get(Msisdn) of
        #soap_response{status=0}=R ->
            {ok, Fmt} = application:get_env(msg_reg_get_ok),
            Msg = lists:flatten(io_lib:format(Fmt, [Msisdn])),
            util:sms_response(To, R#soap_response{message=Msg});
        #soap_response{status=100}=R ->
            {ok, Fmt} = application:get_env(msg_reg_get_fail),
            Msg = lists:flatten(io_lib:format(Fmt, [Msisdn])),
            util:sms_response(To, R#soap_response{message=Msg});
        R ->
            util:sms_response(To, R)
    end.
