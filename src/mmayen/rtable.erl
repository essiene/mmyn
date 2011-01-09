-module(rtable).
-behaviour(gen_server).
-include("simreg.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
         terminate/2,code_change/3]).

-export([route/2]).

-record(st_rtable, {sep, t}).
-record(sms_req, {from, to, msg}).


init([]) ->
    {ok, #st_rtable{}}.

handle_call(R,_F,St) ->
    {reply, {error, R}, St}.

handle_cast(_,St) ->
    {noreply, St}.

handle_info(_, St) ->
    {noreply, St}.

terminate(_,_) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

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


route(#st_rtable{t=Tbl, sep=S}, #pdu{body=#deliver_sm{source_addr=From, 
            destination_addr=To, short_message=Msg}}) ->
    SmsReq = #sms_req{from=preprocess(From), to=preprocess(To), msg=preprocess(Msg, S)},
    route(Tbl, SmsReq);

route(Tbl, #sms_req{}=SmsReq) ->
    case find_rule(Tbl, SmsReq) of
        {error, norule} ->
            {error, route_not_found};
        {ok, {_,_,_,Handler}=Rule} ->
            case is_allowed(Rule, SmsReq) of
                false ->
                    {error, route_denied};
                true ->
                    {ok, Handler}
            end
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
find_rule([{_,To,Keywords,_}=H|Tail], #sms_req{to=To, msg=Msg}=SmsReq) ->
    case lists:prefix(Keywords, Msg) of
        false ->
            find_rule(Tail, SmsReq);
        true -> 
            {ok, H}
    end;
find_rule([{_,_,_,_}|Tail], #sms_req{to=To}=SmsReq) ->
    find_rule(Tail, SmsReq).

is_allowed({[], _,_,_}, _) ->
    true;
is_allowed({AllowList,_,_,_}, #sms_req{from=From}) -> 
    is_allowed(AllowList, From);
is_allowed([], _) ->
    false;
is_allowed([From|Tail], From) ->
    true;
is_allowed([H|Tail], From) ->
    is_allowed(Tail, From).

