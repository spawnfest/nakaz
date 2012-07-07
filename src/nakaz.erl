-module(nakaz).
-behaviour(application).

%% API

-export([start/0]).
-export([ensure/1, ensure/2, use/2, use/3]).

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

-spec use(atom(), atom()) -> record().
use(Mod, Key) ->
    use(Mod, application:get_application(), Key).

-spec use(atom(), atom(), atom()) -> record().
use(Mod, App, Key) ->
    nakaz_core:use(Mod, App, Key).

%% Application callbacks
%% FIXME(Dmitry): we need to document this behaviour with erl argument
%%                and config
start(_StartType, _StartArgs) ->
    case init:get_argument(nakaz) of
        {ok, [[ConfPath]]} ->
            nakaz_sup:start_link(ConfPath);
        _ ->
            case application:get_env(nakaz, conf_path) of
                {ok, ConfPath} ->
                    nakaz_sup:start_link(ConfPath);
                _ ->
                    %% FIXME(Dmitry): fix error message
                    {error, "please provide a path to config file"}
            end
    end.

stop(_State) ->
    ok.
