-module(http_handler).
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
%               "sms_response" : {
%                               "from":string(),
%                               "to":string(),
%                               "message":string()},
%              }
%
process_response(Body) ->
    process_response(?json_to_record(http_response, Body));
process_response(#http_response{sendresponse_flag=false}=R) ->
    noreply(R);
process_response(#http_response{sms_response=SmsResponse}=R) ->
    Sr = ?json_to_record(SmsResponse),
    reply(R, Sr).

noreply(#http_response{}=R) ->
    {noreply, to_status(R)}.

reply(#http_response{}=R, #sms_response{}=Sr) ->
    {reply, to_response(Sr), to_status(R)}.

to_status(#http_response{success_flag=true, operation=Op, code=C, details=D}) ->
    {ok, {Op, C, D}};
to_status(#http_response{success_flag=false, operation=Op, code=C, details=D}) ->
    {error, {Op, C, D}}.

to_response(#sms_response{from=F, to=T, message=M}) ->
    {F, T, M}.
