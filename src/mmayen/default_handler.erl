-module(default_handler).
-export([noreply/5, echo/5, hello/5]).


noreply(_, _, _, _, _) ->
    {noreply, ok}.

echo(_Tid, From, To, _Keywords, Msg) ->
    {reply, {To, From, string:join(Msg, " ")}, {ok, {echo, 0}}}.

hello(_Tid, From, To, _Keywords, _Msg) ->
    {reply, {To, From, "Hello mmyn world!"}, {ok, {hello, 0}}}.
