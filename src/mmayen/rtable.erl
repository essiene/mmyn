-module(rtable).
-behaviour(gen_server).

-export([route/1]).

-record(st_rtable, {t}).
-record(sms_req, {from, to, msg}).

%% Routing Table Structure
% rtable() = [rule()]
% rule() = {deny(), destaddr(), keywords(), handler()}
% deny() = [iolist()]
% destaddr() = iolist()
% keywords() = [iolist()]
% handler() = iolist() | {atom(), atom()}
%
% And example is shown below
%
% [ 
%   {[], 
%       "789", 
%       ["reg", "bar"], 
%       "http://foo.bar/service" 
%   }, 
%   {
%       ["07062022125", "MTNN"], 
%       "33923", 
%       ["reg", "bar"], 
%       "http://foo.bar/service" 
%   }
% ]


route(Tbl, Seperator, #pdu{body=#deliver_sm{source_addr=From, 
            destination_addr=To, short_message=Msg}}) ->

    SmsReq = #sms_req{from=preprocess(From), to=preprocess(To), msg=preprocess(Msg, Seperator)},

    route(Tbl, SmsReq).


route(Tbl, #sms_req{to=To}) ->
    case find_rule(Tbl, To) of
        {error, norule} ->
            {error, route_not_found};
        {ok, Rule} ->
            %do something with rule
            ok
    end.


preprocess(Msg0) ->
    Msg = string:strip(Msg0),
    string:to_lower(Msg).

preprocess(Msg0, Seperator) ->
    Msg = string:strip(Msg0),
    Lower = string:to_lower(Msg),
    string:tokens(Lower, Seperator).

find_rule([], _) ->
    {error, norule};
find_rule([{_,To,_,_}=H|_], To) ->
    {ok, H};
find_rule([{_,_,_,_}|Tail], To) ->
    find_rule(Tail, To).

