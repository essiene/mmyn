-module(default_handler).
-export([noreply/4, echo/4, hello/4]).


noreply(_, _, _, _) ->
    {noreply, ok}.

echo(From, To, _Keywords, Msg) ->
    {reply, {To, From, string:join(Msg, " ")}, ok}.

hello(From, To, _Keywords, _Msg) ->
    {reply, {To, From, "Hello mmyn world!"}, ok}.
