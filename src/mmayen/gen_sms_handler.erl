-module(gen_sms_handler).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {init, 0},
        {handle_sms, 6},
        {terminate, 2}
    ];

behaviour_info(_Other) ->
    undefined.

