-module(notifyjson).
-include_lib("mmynapi/include/mmynapi.hrl").

-export([call/6]).




call(UrlSpec, Tid, Msisdn, Shortcode, Keywords, Msg) ->
    Notify = #'req.notify'{id=Tid, shortcode=list_to_integer(Shortcode), 
        keywords=list_to_binlist(Keywords), msisdn=Msisdn, message=Msg},
    Msg = mmynapi:to_json(?MMYN_SYSTEM, Tid, Notify),

    case mmynapi:call(UrlSpec, Msg) of 
        {error, Reason} ->
            {noreply, {error, {notify, 500, Reason}}};
        {ok, #'mmyn.message'{b=#'mmyn.fault'{code=N, detail=Detail}}} ->
            {noreply, {error, {notify, N, Detail}}};
        {ok, #'mmyn.message'{b=#'res.notify'{status=0, detail=Detail}}} ->
            {noreply, {ok, {notify, 0, Detail}}};
        {ok, #'mmyn.message'{b=#'res.notify'{status=N, detail=Detail}}} ->
            {noreply, {error, {notify, N, Detail}}}
    end.
