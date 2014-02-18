-module(mpns_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(
        StartType :: application:start_type(), 
        StartArgs :: application:start_args() 
    ) ->
        {ok, Pid::pid()} | {error, Reason::any()}.

start(_StartType, _StartArgs) ->
	Module = case application:get_env(mpns, module) of
		{ok, M} -> M;
		_ -> undefined
	end,
    mpns_sup:start_link(Module).

-spec stop( State::any() ) -> ok.

stop(_State) ->
    ok.
