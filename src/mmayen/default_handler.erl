-module(default_handler).
-export([noreply/5, echo/5, hello/5, err_noreply/5, err_reply/5, vsn/5, msg/5]).
-export([os_type/0, os_version/1]).


noreply(_, _, _, _, _) ->
    {noreply, ok}.

echo(_Tid, Msisdn, Scode, _Keywords, Msg) ->
    {reply, {Scode, Msisdn, string:join(Msg, " ")}, {ok, {echo, 0}}}.

hello(_Tid, Msisdn, Scode, _, _) ->
    {reply, {Scode, Msisdn, "Hello mmyn world!"}, {ok, {hello, 0}}}.

msg({Sender, Message}, _Tid, Msisdn, _, _, _) ->
    {reply, {Sender, Msisdn, Message}, {ok, {msg, 0}}}.


err_noreply(_, _, _, _, _) ->
    {noreply, {error, {test, 500, "Test generated error"}}}.

err_reply(_,Msisdn,Scode,_,_) ->
    {reply, 
        {Scode, Msisdn, "Test generated error"},
        {error, {test, 500, "Test generated error"}}
    }.

vsn(_,Msisdn,Scode,_,_) ->
    Eng = "Mmayen",
    Release = "1",
    Os = os_type(),
    Reply = lists:concat(["Eng:", Eng, "\nRel:", Release, "\nOS:", Os]),

    {reply, 
        {To, Msisdn, Reply}, 
        {ok, {vsn, 0}}
    }.


os_type() ->
    case os:type() of
        {Family, Name} ->
            os_version(lists:flatten(io_lib:format("~w/~w", [Family, Name])));
        Family ->
            os_version(lists:flatten(io_lib:format("~w", [Family])))
    end.

os_version(TypeStr) ->
    case os:version() of
        {Maj, Min, Rel} ->
            lists:concat([TypeStr, "/", Maj, ".", Min, ".", Rel]);
        VersionStr ->
            lists:concat([TypeStr, "/", VersionStr])
    end.
