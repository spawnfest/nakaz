-module(nakaz).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0]).

%% API

start() ->
    ok = application:start(nakaz).

%% Application callbacks

start(_StartType, _StartArgs) ->
    {ok, [[ConfPath]]} = init:get_argument(nakaz),
    {ok, Apps} = file:consult(ConfPath),
    io:format("~p~n", [Apps]),
    nakaz_sup:start_link().

stop(_State) ->
    ok.
