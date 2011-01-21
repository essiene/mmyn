-module(rtable).
-behaviour(gen_server).
-include("mmyn.hrl").

-export([start_link/0, select_route/1]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
         terminate/2,code_change/3]).

-export([route/2]).

-record(st_rtable, {sep, t}).
-record(sms_req, {from, to, msg}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

select_route(#pdu{}=Pdu) ->
    % log the time it takes to find a route
    gen_server:call(?MODULE, {select_route, Pdu});
select_route(_) ->
    {error, non_routable_data}.

init([]) ->
    {Separators, Table} = util:routing_params(),
    {ok, #st_rtable{sep=Separators, t=Table}}.

handle_call({select_route, Pdu}, _, St) ->
    {reply, route(St, Pdu), St};

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
        {ok, {_,_,_,Handler}=Rule, RouteData} ->
            case is_allowed(Rule, #sms_req{from=From}=SmsReq) of
                false ->
                    {error, route_denied};
                true ->
                    {ok, Handler, RouteData#route_data{from=From}}
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
    %if rule has no keywords, once the shortcode matches, it's a match
    case length(Keywords) of
        0 ->
            Rd = route_data_new(To, Keywords, Msg),
            {ok, H, Rd};
        _ ->
            %check if SMS contains specified keywords
            case lists:prefix(Keywords, Msg) of
                false ->
                    find_rule(Tail, SmsReq);
                true -> 
                    Rd = route_data_new(To, Keywords, Msg),
                   {ok, H, Rd}
            end
    end;
find_rule([{_,_,_,_}|Tail], SmsReq) ->
    find_rule(Tail, SmsReq).

is_allowed({[], _,_,_}, _) ->
    true;
is_allowed({AllowList,_,_,_}, #sms_req{from=From}) -> 
    is_allowed(AllowList, From);
is_allowed([], _) ->
    false;
is_allowed([From|_], From) ->
    true;
is_allowed([_|Tail], From) ->
    is_allowed(Tail, From).

route_data_new(To, Keywords, Msg) -> 
    Msg0 = lists:sublist(Msg, length(Keywords)+1, length(Msg)), 
    #route_data{to=To, keywords=Keywords, msg=Msg0}.
 
