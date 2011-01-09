-module(http_handler).
-include("simreg.hrl").
-include("jsonerl.hrl").
-export([get/6]).


%% will call 
get(Url0, Tid, From, To, Keywords, Msg) ->
    Kw = string:join(Keywords, "+"),
    Data  = string:join(Msg, "+"),

    Url1 = io_lib:format("~s?tid=~s&from=~s&to=~s&keywords=~s&data=~s", [
            Url0,Tid,From,To,Kw,Data]),

    Url2 = lists:flatten(Url1),

    fetch_url(Url2).

    

fetch_url(Url) ->
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _, Response} ->
            process_response(Response);
        {ok, HttpStatus, _, _} ->
            Code = list_to_integer(HttpStatus),
            {noreply, {error, {http, Code, Url}}};
        {error, Reason} ->
            Detail = io_lib:format("Ibrowse Error: ~p", [Reason]),
            {noreply, {error, {ibrowse, 500, Detail}}}
    end.

%%
% The external process is expected to return a simple JSON
% String that will be converted back to the response structure
%
% response() =  {
%               "success_flag" : boolean(),
%               "operation" : string(),
%               "code": integer(),
%               "details": string(),
%               "sendresponse_flag": boolean(),
%               "from":string(),
%               "to":string(),
%               "message":string()
%              }
%
process_response(#http_response{sendresponse_flag=false}=R) ->
    noreply(R);
process_response(#http_response{}=R) ->
    reply(R);
process_response(Body) ->
    process_response(?json_to_record(http_response, Body)).


noreply(#http_response{}=R) ->
    {noreply, to_status(R)}.

reply(#http_response{}=R) ->
    {reply, to_response(R), to_status(R)}.

to_status(#http_response{success_flag=true, operation=Op, code=C, details=D}) ->
    {ok, {binary_to_list(Op), C, binary_to_list(D)}};
to_status(#http_response{success_flag=false, operation=Op, code=C, details=D}) ->
    {error, {binary_to_list(Op), C, binary_to_list(D)}}.

to_response(#http_response{from=F, to=T, message=M}) ->
    {binary_to_list(F), binary_to_list(T), binary_to_list(M)}.
