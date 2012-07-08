-module(nakaz).
-behaviour(application).
-include("nakaz_internal.hrl").

%% API

-export([start/0, stop/0]).
-export([ensure/2, ensure/3, ensure/4,
         use/2, use/3]).

%% Application callbacks

-export([start/2, stop/1]).

%% Types

-type nakaz_option()  :: {reload_type, reload_type()}
                       | {nakaz_loader, module()}.
-type nakaz_options() :: [nakaz_option()].

%% API

start() ->
    ok = application:start(nakaz).

stop() ->
    ok = application:stop(nakaz).

%% FIXME(Dmitry): document this clever hack with record() that ensures
%%                record presence in this module
%% FIXME(Dmitry): check that records provided are actually tuples with
%%                size >= 1
-spec ensure(atom(), [record_()]) -> ret_novalue().
ensure(Mod, Records) ->
    ensure(Mod, Records, []).

-spec ensure(atom(), [record_()], nakaz_options()) -> ret_novalue().
ensure(Mod, Records, Options) ->
    {ok, AppName} = application:get_application(),
    ensure(Mod, AppName, Records, Options).

-spec ensure(atom(), atom(), [record_()], nakaz_options()) -> ret_novalue().
ensure(Mod, App, Records, Options) ->
    nakaz_core:ensure(Mod, App, Records, Options).

-spec use(atom(), T) -> ret_value(T) when T :: record_().
use(Mod, Record) ->
    {ok, AppName} = application:get_application(),
    use(Mod, AppName, Record).

-spec use(atom(), atom(), T) -> ret_value(T) when T :: record_().
use(Mod, App, Record) ->
    nakaz_core:use(Mod, App, Record).

%% Application callbacks

%% FIXME(Dmitry): we need to document this behaviour with erl argument
%%                and config
start(_StartType, _StartArgs) ->
    case init:get_argument(nakaz) of
        {ok, [[ConfPath]]} ->
            nakaz_sup:start_link(ConfPath);
        {ok, _} ->
            {error, "nakaz requires a single configuration file"};
        error ->
            case application:get_env(nakaz, config) of
                {ok, ConfPath} ->
                    nakaz_sup:start_link(ConfPath);
                _ ->
                    %% FIXME(Dmitry): fix error message
                    {error, "please provide a path to config file"}
            end
    end.

stop(_State) ->
    ok.
