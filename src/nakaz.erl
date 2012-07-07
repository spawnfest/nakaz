-module(nakaz).
-behaviour(application).

%% API

-export([start/0]).
-export([ensure/1, ensure/2, use/1, use/2]).

%% Application callbacks

-export([start/2, stop/1]).

%% API

%% API

start() ->
    ok = application:start(nakaz).

%% Types

-type nakaz_option()  :: undefined.
-type nakaz_options() :: [nakaz_option()].

%% API

-spec ensure([atom()]) -> ok.
ensure(RecordNames) ->
    ensure(RecordNames, []).

-spec ensure([atom()], nakaz_options()) -> ok.
ensure(_RecordNames, _Options) ->
    ok.

-spec use(atom()) -> ok.
use(Key) ->
    use(application:get_application(), Key).

-spec use(atom(), atom()) -> ok.
use(_App, _Key) ->
    ok.

%% Application callbacks

start(_StartType, _StartArgs) ->
    {ok, [[ConfPath]]} = init:get_argument(nakaz),
    {ok, Apps} = file:consult(ConfPath),
    io:format("~p~n", [Apps]),
    nakaz_sup:start_link().

stop(_State) ->
    ok.
