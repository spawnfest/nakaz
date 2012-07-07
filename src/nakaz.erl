-module(nakaz).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([start/0]).
-export([ensure/2, ensure/1, use/1]).

%% API

start() ->
    ok = application:start(nakaz).

ensure(RecordNames) ->
    ensure(RecordNames, async).

ensure(RecordNames, ReloadType) ->
    {ok, RecordNames, ReloadType}.

use(Entry) ->
    {ok, Entry}.

%% Application callbacks

start(_StartType, _StartArgs) ->
    {ok, [[ConfPath]]} = init:get_argument(nakaz),
    {ok, Apps} = file:consult(ConfPath),
    io:format("~p~n", [Apps]),
    nakaz_sup:start_link().

stop(_State) ->
    ok.
