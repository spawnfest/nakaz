-module(nakaz_core).
-behaviour(gen_server).
-include("nakaz_internal.hrl").
-compile([{parse_transform, lager_transform}]).

%% FIXME(Sergei): remove!
-compile(export_all).


%% API
-export([start_link/1]).
-export([ensure/4, use/3, reload/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {config_path :: string(),
                reload_type :: reload_type()}).

%%% API
%% FIXME(Dmitry): add typespecs

start_link(ConfPath) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ConfPath], []).

ensure(Mod, App, Records, Options) ->
    gen_server:call(?SERVER, {ensure, Mod, App, Records, Options}).

use(Mod, App, Record) ->
    gen_server:call(?SERVER, {use, Mod, App, Record}).

reload() ->
    gen_server:call(?SERVER, reload).

%%% gen_server callbacks
init([ConfPath]) ->
    ets:new(nakaz_apps, [named_table]),
    ets:new(nakaz_registry, [named_table, bag]),
    {ok, #state{config_path=ConfPath}}.

handle_call({ensure, Mod, App, Records, Options}, _From, State) ->
    ReloadType = proplists:get_value(reload_type, Options, async),
    case read_config(State#state.config_path, Mod, App, Records) of
        {error, _Reason}=E ->
            {reply, E, State};
        ok ->
            {reply, ok, State#state{reload_type=ReloadType}}
    end;
handle_call({use, Mod, App, Record}, _From, State) ->
    RecordName = erlang:element(1, Record),
    ets:insert(nakaz_registry, {{App, RecordName}, Mod}),
    {reply, ok, State};
handle_call(Request, _From, State) ->
    lager:warning("Unhandled call ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("Unhandled info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

read_config(ConfPath, _Mod, _App, _Records) ->
    read_config_file(ConfPath).

read_config_file(ConfPath) ->
    %% FIXME(Dmitry): rewrite this with Z_VALIDATE
    %% FIXME(Dmitry): all errors should be in human-readable format:
    %%                for example, file:read_file returns atoms like enoent
    case file:read_file(ConfPath) of
        {ok, Content} ->
            case yaml_libyaml:binary_to_libyaml_event_stream(Content) of
                {ok, Events} ->
                    case nakaz_composer:compose(Events) of
                        {ok, RawConfig} -> check_config_structure(RawConfig);
                        {error, _Reason}=Error -> Error
                    end;
                {error, _Reason}=Error -> Error
            end;
        {error, _Reason}=Error -> Error
    end.

check_config_structure(RawConfig) ->
    %% FIXME(Dmitry): this function should check that config actually has
    %%                two levels
    {ok, RawConfig}.



%% FIXME(Dmitry): add error rendering
