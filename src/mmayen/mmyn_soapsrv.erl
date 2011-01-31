%%%-------------------------------------------------------------------
%%% Created : 29 Nov 2006 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Author  : Willem de Jong (w.a.de.jong@gmail.com).
%%% Desc    : A SOAP server.
%%% Modified By: Essien Ita Essien (essiene@gmail.com).
%%%
%%% This server is a direct modification of the detergent_server.erl
%%% module found in the detergent project at 
%%%            
%%%     http://github.com/devinus/detergent.git
%%%
%%%-------------------------------------------------------------------
-module(mmyn_soapsrv).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         setup/3, setup/4,
         dispatch/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("detergent/include/detergent.hrl").

-define(SERVER, ?MODULE).

-record(soap_endpoint, {name, mf, wsdl}). % name = atom(), mf = {Module, Function},
                                          % wsdl = #wsdl{}

%% State
-record(s, {
          endpoint_list = []
         }).

-define(OK_CODE, 200).
-define(BAD_MESSAGE_CODE, 400).
%% -define(METHOD_NOT_ALLOWED_CODE, 405).
-define(SERVER_ERROR_CODE, 500).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	start_link([]).
start_link(L) ->
    %% We are dependent on erlsom
    case code:ensure_loaded(erlsom) of
        {error, _} -> 
            Emsg = "could not load erlsom",
            error_logger:error_msg("~p: exiting, reason: ~s~n",
                                   [?MODULE, Emsg]),
            {error, Emsg};
        {module, erlsom} ->
            gen_server:start_link({local, ?SERVER}, ?MODULE, [L], [])
    end.

setup(Name, MF, WsdlFile) when is_tuple(MF),size(MF)==2 ->
    Wsdl = detergent:initModel(WsdlFile),
    gen_server:call(?SERVER, {add_endpoint, Name, MF, Wsdl}, infinity).


setup(Name, MF, WsdlFile, Prefix) when is_tuple(MF),size(MF)==2 ->
    Wsdl = detergent:initModel(WsdlFile, Prefix),
    gen_server:call(?SERVER, {add_endpoint, Name, MF, Wsdl}, infinity).

dispatch(Name, SoapAction, Req) ->
    gen_server:call(?SERVER, {dispatch, Name, SoapAction, Req}).



%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([L]) -> %% [ {Name, {Mod,Handler}, WsdlFile} ]
	EndpointList = lists:foldl( fun( SoapSrvMod, OldList) -> 
									setup_on_init( SoapSrvMod, OldList ) 
							end,[],L),
    {ok, #s{endpoint_list = EndpointList}}.

setup_on_init( {Name, MF, WsdlFile}, OldList ) when is_tuple(MF),size(MF)==2 ->
	Wsdl = detergent:initModel(WsdlFile),
    Endpoint = #soap_endpoint{name=Name, mf=MF, wsdl=Wsdl},
    error_logger:info_msg("Added new soap endpoint: ~p~n", [Endpoint]),
	uinsert(Endpoint, OldList);
setup_on_init( {Name, MF, WsdlFile, Prefix}, OldList ) when is_tuple(MF),size(MF)==2 ->
	Wsdl = detergent:initModel(WsdlFile, Prefix),
    Endpoint = #soap_endpoint{name=Name, mf=MF, wsdl=Wsdl},
	uinsert(Endpoint, OldList).
	
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({dispatch, Name, SoapAction, Req}, _From, State) ->
    Reply = dispatch(State, Name, Req, SoapAction),
    {reply, Reply, State};

handle_call({add_endpoint, Name, MF, WsdlModel}, _From, State) ->
    Endpoint = #soap_endpoint{name=Name, mf=MF, wsdl=WsdlModel},
    NewEndpointList = uinsert(Endpoint, State#s.endpoint_list),
    {reply, ok, State#s{endpoint_list = NewEndpointList}};
%%
handle_call(R, _From, State) ->
    {reply, {error, R}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

dispatch(State, Name, SoapAction, {Req, Attachments}) ->
    %%error_logger:info_report([?MODULE, {payload, Req}]),
    case get_endpoint(State, Name) of
        {error, endpoint_not_found} ->
            srv_error(f("Endpoint Not Found: ~p", [Name]));
        {ok, #soap_endpoint{mf={M, F}, wsdl=Wsdl}} ->

            Umsg = (catch erlsom_lib:toUnicode(Req)),
            case catch detergent:parseMessage(Umsg, Wsdl) of
                {ok, Header, Body} -> 
                    %% call function
                    result(Wsdl, catch apply(M, F, [SoapAction, Header, Body, Attachments]));
                {error, Error} ->
                    cli_error(Error);
                OtherError -> 
                    srv_error(f("Error parsing message: ~p", [OtherError]))
            end
    end;

dispatch(State, Name, SoapAction, Req) ->
    %%error_logger:info_report([?MODULE, {payload, Req}]),
    case get_endpoint(State, Name) of
        {error, endpoint_not_found} ->
            srv_error(f("Endpoint Not Found: ~p", [Name]));
        {ok, #soap_endpoint{mf={M, F}, wsdl=Wsdl}} ->

            Umsg = (catch erlsom_lib:toUnicode(Req)),
            case catch detergent:parseMessage(Umsg, Wsdl) of
                {ok, Header, Body} -> 
                    %% call function
                    result(Wsdl, catch apply(M, F, [SoapAction, Header, Body]));
                {error, Error} ->
                    cli_error(Error);
                OtherError -> 
                    srv_error(f("Error parsing message: ~p", [OtherError]))
            end
    end.


%%% Analyse the result and produce some output
result(Model, {ok, ResBody}) ->
    return(Model, undefined, ResBody, ?OK_CODE, undefined);
result(Model, {ok, ResHeader, ResBody}) ->
    return(Model, ResHeader, ResBody, ?OK_CODE, undefined);
result(Model, {ok, ResHeader, ResBody, Files}) ->
    return(Model, ResHeader, ResBody, ?OK_CODE, Files);
result(_Model, {error, ClientMssg}) ->
    cli_error(ClientMssg);
result(_Model, false) ->   % soap notify !
    false;
result(_Model, Error) ->
    srv_error(f("Error processing message: ~p", [Error])).

return(#wsdl{model = Model}, ResHeader, ResBody, ResCode, Files) ->
    return(Model, ResHeader, ResBody, ResCode, Files);
return(Model, ResHeader, ResBody, ResCode, Files) when not is_list(ResBody) ->
    return(Model, ResHeader, [ResBody], ResCode, Files);
return(Model, ResHeader, ResBody, ResCode, Files) ->
    %% add envelope
    Header2 = case ResHeader of
                  undefined -> undefined;
                  _         -> #'soap:Header'{choice = ResHeader}
              end,
    Envelope = #'soap:Envelope'{'Body' =  #'soap:Body'{choice = ResBody},
                                'Header' = Header2},
    case catch erlsom:write(Envelope, Model) of
        {ok, XmlDoc} ->
	    case Files of
		undefined ->
		    {ok, XmlDoc, ResCode};
		_ ->
		    DIME = detergent_dime:encode(XmlDoc, Files),
		    {ok, DIME, ResCode}
	    end;
        {error, WriteError} ->
            srv_error(f("Error writing XML: ~p", [WriteError]));
        OtherWriteError ->
            error_logger:error_msg("~p(~p): OtherWriteError=~p~n", 
                                   [?MODULE, ?LINE, OtherWriteError]),
            srv_error(f("Error writing XML: ~p", [OtherWriteError]))
    end.

f(S,A) -> lists:flatten(io_lib:format(S,A)).

cli_error(Error) -> 
    error_logger:error_msg("~p(~p): Cli Error: ~p~n", 
                           [?MODULE, ?LINE, Error]),
    Fault = detergent:makeFault("Client", f("~p", [Error])),
    {error, Fault, ?BAD_MESSAGE_CODE}.

srv_error(Error) -> 
    error_logger:error_msg("~p(~p): Srv Error: ~p~n", 
                           [?MODULE, ?LINE, Error]),
    Fault = detergent:makeFault("Server", f("~p", [Error])),
    {error, Fault, ?SERVER_ERROR_CODE}.


get_endpoint(State, Name) ->
    case lists:keysearch(Name, 2, State#s.endpoint_list) of
        {value, EndPoint} -> {ok, EndPoint};
        _                   -> {error, endpoint_not_found}
    end.

uinsert({K,_} = E, [{K,_}|T]) -> [E|T];
uinsert(E, [H|T])             -> [H|uinsert(E,T)];
uinsert(E, [])                -> [E].




