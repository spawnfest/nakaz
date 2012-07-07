-module(nakaz_core).
-behaviour(gen_server).
-include("nakaz_internal.hrl").
-compile([{parse_transform, lager_transform}]).

-include_lib("z_validate/include/z_validate.hrl").

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
    ConfResult = read_config(State#state.config_path, Mod, App, Records),
    io:format("ConfResult: ~p", [ConfResult]),
    case read_config(State#state.config_path, Mod, App, Records) of
        {error, _Reason}=E ->
            {reply, E, State};
        {ok, _} ->
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

read_config(ConfPath, Mod, App, _Records) ->
    %% read record specs from mod
    %% read file
    %% verify presence of application
    %% verify presence of all records
    %% typecheck all records
    %% FIXME(Dmitry): use MERG on the list above, for God's sake
    try
        _RecSpecs = myz_verify_ok(catch Mod:?NAKAZ_MAGIC_FUN(),
                                 {cant_execute_magic_fun, Mod}),
        ConfFile = myz_verify_ok(read_config_file(ConfPath)),
        AppConf = case proplists:get_value(App, ConfFile) of
                      undefined ->
                          ?Z_THROW({no_entry_for_app, App});
                      T -> T
                  end,
        z_return(AppConf)
    catch
        ?Z_OK(Result) -> {ok, Result};
        ?Z_ERROR(Error) -> {error, Error}
    end.

read_config_file(ConfPath) ->
    %% FIXME(Dmitry): all errors should be in human-readable format:
    %%                for example, file:read_file returns atoms like enoent
    try
        RawConfFile = myz_verify_ok(
                        file:read_file(ConfPath)),
        Events = myz_verify_ok(
                   yaml_libyaml:binary_to_libyaml_event_stream(RawConfFile)),
        %% FIXME(Dmitry): add error rendering.
        RawConfig = myz_verify_ok(nakaz_composer:compose(Events)),
        ConfFile  = myz_verify_ok(check_config(RawConfig)),
        z_return(ConfFile)
    catch
        ?Z_OK(Result) -> {ok, Result};
        ?Z_ERROR(Error) -> {error, Error}
    end.

myz_verify_ok(Val) ->
    case Val of
        {ok, ValOk}     -> ValOk;
        {error, Reason} -> ?Z_THROW(Reason);
        Other           -> ?Z_THROW({unknown_value, Other})
    end.

myz_verify_ok(Val, Err) ->
    case Val of
        {ok, ValOk}     -> ValOk;
        {error, Reason} -> ?Z_THROW(Reason);
        _               -> ?Z_THROW(Err)
    end.

check_config([{RawConfig, _pos}]) ->
    case check_config_apps(RawConfig) of
        [] -> {ok, RawConfig};
        Malformed -> {error, {malformed, Malformed}}
    end;
check_config(_RawConfig) ->
    {error, 'wtf_dawg?'}.

check_config_apps(RawConfig) ->
    check_config_apps(RawConfig, []).

check_config_apps([], Acc) ->
    lists:flatten(Acc);
check_config_apps([{App, {[_|_]=Block, _Pos}}|RawConfig], Acc)
  when is_atom(App) ->
    check_config_apps(RawConfig,
                      [check_config_sections(Block)|Acc]);
check_config_apps([App|RawConfig], Acc) ->
    check_config_apps(RawConfig, [{app, App}|Acc]).

check_config_sections(Sections) ->
    check_config_sections(Sections, []).

check_config_sections([], Acc) ->
    lists:reverse(Acc);
check_config_sections([{Section, {[_|_], _Pos}}|Sections], Acc)
  when is_atom(Section) ->
    check_config_sections(Sections, Acc);
check_config_sections([Section|Sections], Acc) ->
    check_config_sections(Sections, [{section, Section}|Acc]).
