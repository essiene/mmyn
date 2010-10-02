-module(rebarctl).
-export([admin/1]).

admin(["txqping"]) ->
    {reply, txq:ping()};
admin(["txstat"]) ->
    {reply, tx_sup:ping()};
admin(["rxstat"]) ->
    {reply, rx_sup:ping()};
admin(["startrx"]) ->
    {reply, rx_sup:start_child(1)};
admin(_) ->
    {reply, help()}.


help() ->
    [{txqping, "Check transmit queue status"}, 
     {txstat, "Check transmitters status"},
     {rxstat, "Check receivers status"}].
