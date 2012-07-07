-module(nakaz_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type),
        {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args),
        {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% API functions

start_link(ConfPath) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ConfPath]).

%% Supervisor callbacks

init([ConfPath]) ->
    {ok, {{one_for_one, 5, 10},
          [?CHILD(nakaz_core, worker, [ConfPath])]}}.
