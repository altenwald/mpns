-module(mpns_app).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, A), {I, {I, start_link, A}, transient, 5000, worker, [I]}).

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
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Module]).

-spec stop( State::any() ) -> ok.

stop(_State) ->
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init(Args::[]) ->
    {ok,{{
        RestartStrategy::supervisor:strategy(),
        MaxR::non_neg_integer(),
        MaxT::non_neg_integer()},
        [ChildSpec::supervisor:child_spec()]}} | 
    ignore.

init([Module]) ->
    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(mpns, [Module])
    ]}}.
