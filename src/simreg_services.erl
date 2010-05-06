-module(simreg_services).
-include("simreg.hrl").

-export([handle_sms/3, msisdn_strip/2]).


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
    simreg_tx:sendsms("mmyn", Src, "eng: Mmayen\nvsn: 1.0\nos: Solaris 10");

handle_sms(Src, "789", ["help" | _Rest]) ->
    util:sms_response(Src, "help) Menu\npuk) Get puk\nreg)Get status");

handle_sms(Src, "789", ["puk", _PUK | _Rest]) ->
    util:sms_response(Src, "Your supplied PUK is correct");

handle_sms(Src, "789", ["puk" | _Rest]) ->
    Res = puk:get(Src),
    util:sms_response(Src, Res);

handle_sms(Src, "789", ["reg" , Msisdn0 | _Rest]) ->
    Msisdn1 = msisdn_strip(Msisdn0, 5),
    Msisdn = string:concat("234", Msisdn1),

    case regstatus:get(Msisdn) of
        #soap_response{status=0}=R ->
            Msg = string:concat(Msisdn, " is fully registered on the MTNN network"),
            util:sms_response(Src, R#soap_response{message=Msg});
        #soap_response{status=100}=R ->
            Msg = string:concat(Msisdn, " is NOT YET registered on the MTNN network"),
            util:sms_response(Src, R#soap_response{message=Msg});
        R ->
            util:sms_response(Src, R)
    end;

handle_sms(Src, "789", ["reg" | _Rest]) ->
    case regstatus:get(Src) of
        #soap_response{status=0}=R ->
            Msg = "Your SIM is fully registered on the MTNN network",
            util:sms_response(Src, R#soap_response{message=Msg});
        #soap_response{status=100}=R ->
            Msg = "Your SIM is NOT YET registered on the MTNN network",
            util:sms_response(Src, R#soap_response{message=Msg});
        R ->
            util:sms_response(Src, R)
    end;

handle_sms(Src, Dst, WordList) ->
    error_logger:info_report([unhandled_sms, {src, Src}, {dst, Dst}, {wordlist, WordList}]).
