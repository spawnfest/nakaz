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

-record(state, {config_path  :: string(),
                reload_type  :: reload_type(),
                nakaz_loader :: module(),
                nakaz_ensurer :: module()}).

%%% API
%% FIXME(Dmitry): add typespecs
%% FIXME(Dmitry): spec
start_link(ConfPath) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ConfPath], []).

%% FIXME(Dmitry): spec
ensure(Mod, App, Records, Options) ->
    gen_server:call(?SERVER, {ensure, Mod, App, Records, Options}).

%% FIXME(Dmitry): spec
use(Mod, App, Record) ->
    gen_server:call(?SERVER, {use, Mod, App, Record}).

%% FIXME(Dmitry): spec
reload() ->
    gen_server:call(?SERVER, reload).

%%% gen_server callbacks
init([ConfPath]) ->
    nakaz_registry = ets:new(nakaz_registry, [named_table, bag]),
    {ok, #state{config_path=ConfPath}}.

handle_call({ensure, Mod, App, Records, Options}, _From, State) ->
    %% FIXME(Dmitry): we should use different reload types for different
    %%                apps
    ReloadType = proplists:get_value(reload_type, Options, async),
    NakazLoader = proplists:get_value(nakaz_loader, Options, undefined),
    case read_config(State#state.config_path,
                     Mod, App, Records, NakazLoader, Mod) of
        {error, Reason} ->
            {reply, {error, nakaz_errors:render(Reason)}, State};
        {ok, T} ->
            io:format("ReadConf result: ~p~n", [T]),
            {reply, ok, State#state{reload_type=ReloadType,
                                    nakaz_loader=NakazLoader,
                                    nakaz_ensurer=Mod}}
    end;
handle_call({use, Mod, App, Record}, _From, State) ->
    case read_config(State#state.config_path,
                     Mod, App, [Record],
                     State#state.nakaz_loader,
                     State#state.nakaz_ensurer) of
        {error, Reason} ->
            {reply, {error, nakaz_errors:render(Reason)}, State};
        {ok, [Config]} ->
            RecordName = erlang:element(1, Record),
            ets:insert(nakaz_registry, {{App, RecordName}, Mod, Config}),
            {reply, {ok, Config}, State}
    end;
handle_call(reload, _From, #state{reload_type=ReloadType,
                                  nakaz_loader=NakazLoader,
                                  nakaz_ensurer=Ensurer}=State) ->
    %% FIXME(Dmitry): implement sync reload strategy
    %% FIXME(Dmitry): RecordName/Section controversy should be ended
    case reload_config(State#state.config_path, ReloadType,
                       NakazLoader, Ensurer) of
        {error, Reason} ->
            {reply, {error, nakaz_errors:render(Reason)}, State};
        ok -> {reply, ok, State}
    end;
handle_call(Request, _From, State) ->
    ok = lager:warning("Unhandled call ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ok = lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ok = lager:warning("Unhandled info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

%% implement sync reload
%% FIXME(Dmitry): check that record was passed to ensure before use
reload_config(ConfPath, async, NakazLoader, Ensurer) ->
    try
        Registry = ets:tab2list(nakaz_registry),
        AllConfigs =
            [begin
                 [NewConfig] = myz_verify_ok(read_config(ConfPath, Mod, App,
                                                         [RecordName],
                                                         NakazLoader,
                                                         Ensurer)),
                 {Mod, OldConfig, NewConfig}
             end || {{App, RecordName}, Mod, OldConfig} <- Registry],
        %% FIXME(Dmitry): definitely not the most effective way to compare
        %%                configs
        NewConfigs = [{Mod, NewConfig}
                      || {Mod, OldConfig, NewConfig} <- AllConfigs,
                         NewConfig /= OldConfig],
        _ = [case Mod:nakaz_check(Config) of
                 ok -> ok;
                 {error, Reason} -> ?Z_THROW({config_check_failed, Mod, Reason})
             end || {Mod, Config} <- NewConfigs],
        _ = [case Mod:nakaz_load(Config) of
                 ok -> ok;
                 {error, Reason} -> ?Z_THROW({config_load_failed, Mod, Reason})
             end || {Mod, Config} <- NewConfigs],
        z_return(ok)
    catch
        ?Z_OK(_) -> ok;
        ?Z_ERROR(Error) -> {error, Error}
    end.

%% FIXME(Dmitry): spec
read_config(ConfPath, _Mod, App, Records, NakazLoader, Ensurer) ->
    %% read record specs from mod
    %% read file
    %% verify presence of application
    %% verify presence of all records (?)
    %% typecheck all records
    %% FIXME(Dmitry): use MERG on the list above, for God's sake
    try
        RecSpecs = myz_verify_ok(
                     catch Ensurer:?NAKAZ_MAGIC_FUN(),
                     {cant_execute_magic_fun, Ensurer}),
        ConfFile = myz_verify_ok(
                     read_config_file(ConfPath)),
        {AppConf, _AppPos} = myz_defined(
                              proplists:get_value(App, ConfFile),
                              {missing, {app, App}}),
        ConfRecs =
            [begin
                 RecName = case is_tuple(Record) of
                               true -> erlang:element(1, Record);
                               false when is_atom(Record) -> Record
                           end,
                 RawConfSection = myz_defined(
                                    proplists:get_value(RecName, AppConf),
                                    {missing, {section, RecName}}),
                 myz_verify_ok(
                   nakaz_typer:type(RecName, RawConfSection, RecSpecs,
                                    NakazLoader))
             end || Record <- Records],
        z_return(ConfRecs)
    catch
        ?Z_OK(Result) -> {ok, Result};
        ?Z_ERROR(Error) -> {error, Error}
    end.

%% FIXME(Dmitry): spec
%% FIXME(Dmitry): merge this into previous fun?
read_config_file(ConfPath) ->
    %% FIXME(Dmitry): all errors should be in human-readable format:
    %%                for example, file:read_file returns atoms like enoent
    try
        RawConfFile = myz_verify_ok(
                        file:read_file(ConfPath)),
        Events = myz_verify_ok(
                   yaml_libyaml:binary_to_libyaml_event_stream(RawConfFile)),
        RawConfig = myz_verify_ok(nakaz_composer:compose(Events)),
        ConfFile  = myz_verify_ok(check_config(RawConfig)),
        z_return(ConfFile)
    catch
        ?Z_OK(Result) -> {ok, Result};
        ?Z_ERROR(Error) -> {error, Error}
    end.

%% FIXME(Dmitry): into z_validate?
myz_verify_ok(Val) ->
    case Val of
        {ok, ValOk}     -> ValOk;
        {error, Reason} -> ?Z_THROW(Reason);
        Other           -> ?Z_THROW({unknown_value, Other})
    end.

%% FIXME(Dmitry): into z_validate?
myz_verify_ok(Val, Err) ->
    case Val of
        {ok, ValOk}     -> ValOk;
        {error, Reason} -> ?Z_THROW(Reason);
        _               -> ?Z_THROW(Err)
    end.

%% FIXME(Dmitry): into z_validate?
myz_defined(Val, Err) ->
    case Val of
        undefined -> ?Z_THROW(Err);
        _         -> Val
    end.

%% FIXME(Dmitry): spec
check_config([{RawConfig, _pos}]) ->
    case check_config_apps(RawConfig) of
        [] -> {ok, RawConfig};
        Malformed -> {error, {malformed, Malformed}}
    end.

%% FIXME(Dmitry): spec
check_config_apps(RawConfig) ->
    check_config_apps(RawConfig, []).

%% FIXME(Dmitry): spec
check_config_apps([], Acc) ->
    lists:flatten(Acc);
check_config_apps([{App, {[_|_]=Block, _Pos}}|RawConfig], Acc)
  when is_atom(App) ->
    check_config_apps(RawConfig,
                      [check_config_sections(Block)|Acc]);
check_config_apps([App|RawConfig], Acc) ->
    check_config_apps(RawConfig, [{app, App}|Acc]).

%% FIXME(Dmitry): spec
check_config_sections(Sections) ->
    check_config_sections(Sections, []).

%% FIXME(Dmitry): spec
check_config_sections([], Acc) ->
    lists:reverse(Acc);
check_config_sections([{Section, {[_|_], _Pos}}|Sections], Acc)
  when is_atom(Section) ->
    check_config_sections(Sections, Acc);
check_config_sections([Section|Sections], Acc) ->
    check_config_sections(Sections, [{section, Section}|Acc]).
