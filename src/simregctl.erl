-module(simregctl).
-export([
        action/0,
        action/1
    ]).


action([exit]) ->
    simreg_exec(init, stop, []),
    display({simreg, "Exited"});

action([stop]) ->
    simreg_exec(simreg, stop, []),
    display({simreg, "Stopped"});

action([start]) ->
    simreg_exec(simreg, start, []),
    display({simreg, "Started"});

action([status]) ->
    display(simreg_exec(simreg_manager, status, []));

action(Any) ->
    available_commands(Any).

action() ->
    action([status]).

available_commands(_WrongCommand) ->
    OutString = " \
    ",
    io:format("~s\n", [OutString]).

display([]) ->
    do_nothing;
display({Key, Val}) ->
    ValStr = string:join(["[", Val, "]"], ""),
    KeyValStr = string:join([atom_to_list(Key), ValStr], " - "),
    display(KeyValStr);
display([{_Key, _Val}=Head | Rest]) ->
    display(Head),
    display(Rest);
display(String) ->
    io:format("~s\n", [String]).

simreg_exec(Module, Fun, Args) ->
    SimregNode = get_simreg_node(),
    case rpc:call(SimregNode, Module, Fun, Args) of
        {badrpc, nodedown} ->
            {simreg, "Dead"};
        Response ->
            Response
    end.


get_simreg_node() ->
    [SimregNode] = init:get_plain_arguments(),
    list_to_atom(SimregNode).
