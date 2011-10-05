-module(mmyn_yaws).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,
         code_change/3]).

-record(rcfg, {yaws_gconf, yaws_sconf}).
-define(APPMODS, {appmods, [
            {"v", msisdn_view}
        ]}).
-define(YAWS_NAME, "Mmayen/1.0.0").



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init(_) ->
    case load_config() of
        {error, Reason} ->
            {stop, Reason};
        {ok, Config} ->
            case apply_config(Config) of
                {error, Reason} ->
                    {stop, Reason};
                ok -> 
                    {ok, nil}
            end
    end.

handle_call(R, _F, St) ->
    {reply, {error, R}, St}.

handle_cast(_, St) ->
    {noreply, St}.

handle_info(_, St) ->
    {noreply, St}.

terminate(_,_) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


load_config() ->
    case application:get_env(mmyn, http_env) of
        undefined ->
            {error, {config_not_found, http_env}};
        {ok, HttpEnv0} ->
            SoapMods = [
                {{mmyn_soap, handler}, "var/www/mmyn-2.0.1.wsdl"},
                {{mmyn_soap, notify}, "var/www/notify-2.0.wsdl"}],
            HttpEnv = [{enable_soap, true}, {soap_srv_mods, SoapMods}, {yaws, ?YAWS_NAME}|HttpEnv0],
            Id = proplists:get_value(id, HttpEnv, "mmayen"),
            case application:get_env(mmyn, http_server) of
                undefined ->
                    {error, {config_not_found, http_server}};
                {ok, SrvEnv0} ->
                    SrvEnv = [?APPMODS|SrvEnv0],
                    DocRoot = proplists:get_value(docroot, SrvEnv, "/tmp"),
                    {ok, SconfList, Gconf, _} = yaws_api:embedded_start_conf(DocRoot, SrvEnv, HttpEnv, Id),
                    {ok, #rcfg{yaws_gconf=Gconf,yaws_sconf=SconfList}}
            end
    end.

apply_config(#rcfg{}=R) ->
    yaws_api:setconf(R#rcfg.yaws_gconf, R#rcfg.yaws_sconf).
