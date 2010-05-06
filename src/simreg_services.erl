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

handle_sms(Src, "789", ["mmayen" | _Rest]) ->
    simreg_tx:sendsms("Doo Doo", Src, "Mmayen is the bestest!");

handle_sms(Src, "789", ["help" | _Rest]) ->
    simreg_tx:sendsms("SimReg", Src, "help) Menu\npuk) Get puk\nreg)Get status");

handle_sms(Src, "789", ["puk", _PUK | _Rest]) ->
    simreg_tx:sendsms("SimReg", Src, "Your supplied PUK is correct");

handle_sms(Src, "789", ["puk" | _Rest]) ->
    case puk:get(Src) of
        #soap_response{status=0, puk=PUK} ->
            Msg = string:concat("Your PUK is ", PUK),
            simreg_tx:sendsms("SimReg", Src, Msg);
        #soap_response{status=_N, message=Message} ->
            % TODO: log this as an error, raise snmp trap?
            % send email or sms to notify someone.
            simreg_tx:sendsms("SimReg", Src, "This service is temporarily unavailable. Please try again later"),
            simreg_tx:sendsms("SimReg Error", "2347062022125", Message)
    end;

handle_sms(Src, "789", ["reg" , Msisdn0 | _Rest]) ->
    Msisdn1 = msisdn_strip(Msisdn0, 5),
    Msisdn = string:concat("234", Msisdn1),

    case regstatus:get(Msisdn) of
        #soap_response{status=0} ->
            Msg = string:concat(Msisdn, " is fully registered on the MTNN network"),
            simreg_tx:sendsms("SimReg", Src, Msg);
        #soap_response{status=100} ->
            Msg = string:concat(Msisdn, " is NOT YET registered on the MTNN network"),
            simreg_tx:sendsms("SimReg", Src, Msg);
        #soap_response{status=_N, message=Message} ->
            % TODO: log this as an error, raise snmp trap?
            % send email or sms to notify someone.
            simreg_tx:sendsms("SimReg", Src, "This service is temporarily
                              unavailable. Please try again later"),
            simreg_tx:sendsms("SimReg Error", "2347062022125", Message)
    end;

handle_sms(Src, "789", ["reg" | _Rest]) ->
    case regstatus:get(Src) of
        #soap_response{status=0} ->
            Msg = "Your SIM is fully registered on the MTNN network",
            simreg_tx:sendsms("SimReg", Src, Msg);
        #soap_response{status=100} ->
            Msg = "Your SIM is NOT YET registered on the MTNN network",
            simreg_tx:sendsms("SimReg", Src, Msg);
        #soap_response{status=_N, message=Message} ->
            % TODO: log this as an error, raise snmp trap?
            % send email or sms to notify someone.
            simreg_tx:sendsms("SimReg", Src, "This service is temporarily unavailable. Please try again later"),
            simreg_tx:sendsms("SimReg Error", "2347062022125", Message)
    end;


handle_sms(Src, Dst, WordList) ->
    error_logger:info_report([unhandled_sms, {src, Src}, {dst, Dst}, {wordlist, WordList}]).
