-module(default_handler).
-export([noreply/5, echo/5, hello/5, err_noreply/5, err_reply/5, vsn/5]).


noreply(_, _, _, _, _) ->
    {noreply, ok}.

echo(_Tid, From, To, _Keywords, Msg) ->
    {reply, {To, From, string:join(Msg, " ")}, {ok, {echo, 0}}}.

hello(_Tid, From, To, _Keywords, _Msg) ->
    {reply, {To, From, "Hello mmyn world!"}, {ok, {hello, 0}}}.


err_noreply(_, _, _, _, _) ->
    {noreply, {error, {test, 500, "Test generated error"}}}.

err_reply(_,_,_,_,_) ->
    {reply, 
        {"mmynerr", "Test generated error"},
        {error, {test, 500, "Test generated error"}}
    }.

vsn(_,_,_,_,_) ->
    Eng = "Mmayen",
    Release = "1",
    Os = os_type(),
    Reply = io_lib:format("Eng: ~w\nRel: ~w\nOs: ~w", [Eng, Release, Os]),

    {reply, 
        {"mmyn", lists:flatten(Reply)}, 
        {ok, {vsn, 0}}
    }.


os_type() ->
    case os:type() of
        {Family, Name} ->
            os_version(io_lib:format("~s/~s", [Family, Name]));
        Family ->
            os_version(io_lib:format("~s", [Family]))
    end.

os_version(TypeStr) ->
    case os:version() of
        {Maj, Min, Rel} ->
            lists:flatten([TypeStr, io_lib:format("/~w.~w.~w", [Maj, Min, Rel])]);
        VersionStr ->
            lists:flatten([TypeStr, io_lib:format("/~w", [VersionStr])])
    end.
