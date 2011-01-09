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

    

    %{reply, {To, From, string:join(Msg, " ")}, {ok, {echo, 0}}}.


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

process_response(_) ->
    {noreply, ok}.
