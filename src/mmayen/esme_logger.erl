-module(esme_logger).
-behaviour(gen_event).

-export([init/1,handle_event/2,handle_call/2,
         handle_info/2,terminate/2,code_change/3]).

-record(st_esmelogr, {type, id, logger}).

init([Type, Id]) ->
    {ok, LogDir} = application:get_env(mmyn, esme_logdir),
    {ok, LogSize} = application:get_env(mmyn, esme_logsize),
    {ok, NumRotations} = application:get_env(mmyn, esme_logkeep),
    {ok, Level} = application:get_env(mmyn, esme_loglevel),

    LogFile = create_logfilename(Type, Id),
    Logger = list_to_atom("__" ++ LogFile),
    Suffix = "log",

    AddAppender = fun () ->
        case log4erl:add_file_appender(Logger, Logger, {LogDir, LogFile, {size, LogSize}, NumRotations, Suffix, Level, "%j %T <%L> %l%n"}) of
            {ok, _} ->
                {ok, #st_esmelogr{type=Type, id=Id, logger=Logger}};
            {error, {already_started, _}} ->
                {ok, #st_esmelogr{type=Type, id=Id, logger=Logger}};
            {error, Reason} ->
                {error, Reason}
        end
    end,

    case log4erl:add_logger(Logger) of
        {ok, _} ->
            AddAppender();
        {error, {already_started, _}} ->
            AddAppender();
        {error, Reason} ->
            {error, Reason}
    end.

handle_event({'ERROR'=Level, Log}, #st_esmelogr{logger=Logger}=St) ->
    error_logger:error_msg("~s~n", [Log]),
    log4erl:log(Logger, Level, "~s", [Log]),
    {ok, St};
handle_event({Level, Log}, #st_esmelogr{logger=Logger}=St) ->
    log4erl:log(Logger, Level, "~s", [Log]),
    {ok, St};
handle_event(_, St) ->
    {ok, St}.

handle_call(_, St) ->
    {ok, ok, St}.

handle_info(_, St) ->
    {ok, St}.

terminate(_, #st_esmelogr{logger=Logger}) ->
    log4erl:log(Logger, info, "Logger terminating"),
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


create_logfilename(Type, Id) when is_number(Id) ->
    lists:flatten(io_lib:format("~s~p", [Type, Id]));
create_logfilename(Type, Id) ->
    lists:flatten(io_lib:format("~s~s", [Type, Id])).
