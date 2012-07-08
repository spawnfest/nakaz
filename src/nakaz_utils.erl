-module(nakaz_utils).
-include("nakaz_internal.hrl").
-export([check_config_structure/1]).

%% FIXME(Dmitry): spec
-spec check_config_structure(raw_config()) -> {ok, raw_config()}
                                            | {error, config_structure_error()}.
check_config_structure([]) ->
    {error, empty};
check_config_structure([{RawConfig, _pos}]) ->
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
