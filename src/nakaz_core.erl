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

-record(app, {name          :: atom(),
              loader_mod    :: atom(),
              reload_type   :: sync | async,
              section_names :: [atom()],
              section_spec  :: record_specs()}).

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

reload(App) ->
    gen_server:call(?SERVER, {reload, App}).

%%% gen_server callbacks
init([ConfPath]) ->
    nakaz_registry = ets:new(nakaz_registry, [named_table, bag]),
    nakaz_apps = ets:new(nakaz_apps, [named_table,
                                      {keypos, #app.name}]),
    {ok, #state{config_path=ConfPath}}.

handle_call({ensure, Mod, AppName, Sections, Options}, _From, State) ->
    ReloadType = proplists:get_value(reload_type, Options, async),
    LoaderMod = proplists:get_value(nakaz_loader, Options, undefined),
    ConfigPath = State#state.config_path,

    Result = case catch [element(1, S) || S <- Sections] of
                 Lst when is_list(Lst) ->
                     case catch Mod:?NAKAZ_MAGIC_FUN() of
                         {ok, SectionSpec} ->
                             App = #app{name=AppName,
                                        loader_mod=LoaderMod,
                                        reload_type=ReloadType,
                                        section_names=Lst,
                                        section_spec=SectionSpec},
                             true = ets:insert(nakaz_apps, App),
                             read_config(ConfigPath, AppName, Sections);
                         _ ->
                             {error, {cant_execute_magic_fun, Mod}}
                     end;
                 %% THIS ERROR
                 _ -> {error, sections_arg_should_contain_records}
             end,
    case Result of
        {error, Reason} ->
            {reply, {error, nakaz_errors:render(Reason)}, State};
        {ok, _} ->
            {reply, ok, State}
    end;
handle_call({use, Mod, AppName, Section}, _From, State) ->
    [App] = ets:lookup(nakaz_apps, AppName),
    ConfigPath = State#state.config_path,

    Result = case (is_tuple(Section) andalso
                   lists:member(erlang:element(1, Section),
                                App#app.section_names)) of
                 true ->
                     case read_config(ConfigPath, AppName, [Section]) of
                         {error, _}=Error -> Error;
                         {ok, [Conf]} ->
                             SectionName = erlang:element(1, Section),
                             ets:insert(nakaz_registry,
                                        {{App, SectionName},
                                         Mod, erlang:phash2(Conf)}),
                             {ok, Conf}
                     end;
                 false ->
                     %% THIS ERROR
                     {reply, {error, section_should_be_ensured}}
             end,
    case Result of
        {ok, Config}    -> {reply, Config, State};
        {error, Reason} -> {reply, {error, nakaz_errors:render(Reason)}, State}
    end;
handle_call(reload, _From, State) ->
    ConfigPath = State#state.config_path,

    Results = [case R of
                   {_App, ok}=Ok -> Ok;
                   {App, {error, Reason}} ->
                       {App, {error, nakaz_errors:render(Reason)}}
               end || R <- reload_config(ConfigPath)],
    {reply, Results, State};
handle_call({reload, App}, _From, State) ->
    ConfigPath = State#state.config_path,
    Result = case ets:lookup(nakaz_apps, App) of
                 [_] -> reload_config(ConfigPath, App);
                 %% THIS ERROR
                 []  -> {error, app_is_not_ensured}
             end,
   case Result of
       ok -> {reply, ok, State};
       {error, Reason} -> {reply, {error, nakaz_errors:render(Reason)}, State}
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

reload_config(ConfigPath) ->
    Apps = ets:tab2list(nakaz_apps),
    [{AppName, reload_config(ConfigPath, AppName)}
     || #app{name=AppName} <- Apps].

reload_config(ConfigPath, AppName) ->
    [App] = ets:lookup(nakaz_apps, AppName),

    try
        AllConfigsNested =
            [begin
                 [NewConfig] = zz_verify_ok(read_config(ConfigPath, AppName,
                                                        [SectionName])),
                 [{Mod, NewConfig, OldConfHash /= erlang:phash2(NewConfig)}
                  || {_, Mod, OldConfHash}
                         <- ets:lookup(nakaz_registry, {AppName, SectionName})]
             end || SectionName <- App#app.section_names],
        AllConfigs = lists:flatten(AllConfigsNested),
        NewConfigs = [{Mod, Config}
                      || {Mod, Config, IsNew} <- AllConfigs, IsNew],
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

-spec read_config(string(), atom(),
                  [record_() | atom()]) -> {ok, [record_()]}
                                         | {error, read_config_errors()}.
read_config(ConfPath, AppName, Sections) ->
    %% read record specs from mod
    %% read file
    %% verify presence of application
    %% typecheck all records
    %% FIXME(Dmitry): use MERG on the list above, for God's sake
    [App] = ets:lookup(nakaz_apps, AppName),
    LoaderMod = App#app.loader_mod,
    SectionSpec = App#app.section_spec,

    try
        ConfFile = zz_verify_ok(
                     read_config_file(ConfPath)),
        {AppConf, _AppPos} = zz_defined(
                              proplists:get_value(App#app.name, ConfFile),
                              {missing, {app, App#app.name}}),
        SectionConfs =
            [begin
                 SectionName = case is_tuple(Section) of
                                   true -> erlang:element(1, Section);
                                   false when is_atom(Section) -> Section
                               end,
                 RawSectionConf = zz_defined(
                                    proplists:get_value(SectionName, AppConf),
                                    {missing, {section, SectionName, AppName}}),
                 zz_verify_ok(
                   nakaz_typer:type(SectionName, RawSectionConf,
                                    SectionSpec, LoaderMod))
             end || Section <- Sections],
        z_return(SectionConfs)
    catch
        ?Z_OK(Result) -> {ok, Result};
        ?Z_ERROR(Error) -> {error, Error}
    end.

-spec read_config_file(string()) -> {ok, proplist()}
                                  | {error, read_config_file_errors()}
                                  %% unlikely and undiscriber errors:
                                  | {error, file:posix()}
                                  | {error, untypical_readfile_errors()}.
read_config_file(ConfPath) ->
    %% FIXME(Dmitry): all errors should be in human-readable format:
    %%                for example, file:read_file returns atoms like enoent
    try
        RawConfFile = zz_verify_ok(
                        file:read_file(ConfPath)),
        Events = zz_verify_ok(
                   yaml_libyaml:binary_to_libyaml_event_stream(RawConfFile)),
        RawConfig = zz_verify_ok(
                      nakaz_composer:compose(Events)),
        ConfFile  = zz_verify_ok(
                      nakaz_utils:check_config_structure(RawConfig)),
        z_return(ConfFile)
    catch
        ?Z_OK(Result) -> {ok, Result};
        ?Z_ERROR(Error) -> {error, Error}
    end.

%% z_validate-based funs

-spec zz_verify_ok({ok, A} | {error, any()}) -> A when A :: any().
zz_verify_ok(Val) ->
    case Val of
        {ok, ValOk}     -> ValOk;
        {error, Reason} -> ?Z_THROW(Reason);
        Other           -> ?Z_THROW({unknown_value, Other})
    end.

-spec zz_verify_ok({ok, A} | {error, any()}, any()) -> A when A :: any().
zz_verify_ok(Val, Err) ->
    case Val of
        {ok, ValOk}     -> ValOk;
        {error, Reason} -> ?Z_THROW(Reason);
        _               -> ?Z_THROW(Err)
    end.

-spec zz_defined(A | undefined, any()) -> A when A :: any().
zz_defined(Val, Err) ->
    case Val of
        undefined -> ?Z_THROW(Err);
        _         -> Val
    end.
