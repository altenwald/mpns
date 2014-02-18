
-module(mpns_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, A), {I, {I, start_link, A}, transient, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link(Module::atom()) -> supervisor:startlink_ret().

start_link(Module) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Module]).

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
