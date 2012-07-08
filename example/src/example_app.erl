-module(example_app).
-behaviour(application).
-compile({parse_transform, nakaz_pt}).
-include_lib("nakaz/include/nakaz.hrl").
-include("example.hrl").

%% API
-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% API

start() ->
    ok = application:start(nakaz),
    ok = application:start(example).

stop() ->
    ok = application:stop(example).

%% Application callbacks

start(_StartType, _StartArgs) ->
    Ensure = ?NAKAZ_ENSURE([#srv_conf{}, #log_conf{}],
                           [{nakaz_loader, example_confloader}]),
    io:format("Ensure: ~p~n", [Ensure]),
    ok = Ensure,
    example_sup:start_link().

stop(_State) ->
    ok.
