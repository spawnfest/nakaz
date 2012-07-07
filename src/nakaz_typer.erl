-module(nakaz_typer).
-include("nakaz_internal.hrl").

%% API

-export([type/3]).

%% API

-spec type(atom(), [atom()], raw_config()) -> {ok, [record()]}
                                            | {error, typer_error()}.
type(Mod, Sections, RawConfig) ->
    type_sections(Mod, Sections, RawConfig, []).

%% Internal

type_sections(_Mod, [], _RawConfig, Acc) ->
    {ok, lists:reverse(Acc)};
type_sections(Mod, [Section|Sections], RawConfig, Acc) ->
    %% FIXME(Sergei): is it safe to assume that 'Sections' *always*
    %% matches cases in 'nakaz_magick'.
    Types = Mod:nakaz_magick(Section),
    case proplists:get_value(Section, RawConfig) of
        {ok, {RawSectionConfig, _Pos}} when Types =/= undefined ->
            case type_section(Types, RawSectionConfig) of
                {ok, TypedSectionConfig} ->
                    type_sections(Mod, Sections, RawConfig,
                                  [TypedSectionConfig|Acc]);
                {error, _Reason}=Error   -> Error
            end;
        undefined ->
            {error, {missing_section, Section}}
    end.

type_section(Types, RawSectionConfig) ->
    type_section(Types, RawSectionConfig, []).

type_section([], _RawSectionConfig, Acc) ->
    lists:reverse(Acc);
type_section([{Field, Type, Default}|Types], RawSectionConfig, Acc) ->
    case proplist:get_value(Field, RawSectionConfig) of
        {ok, {RawValue, _Pos}} ->
            case type_field(Type, RawValue) of
                {ok, TypedValue} ->
                    type_section(Types, RawSectionConfig,
                                 [{Field, TypedValue}|Acc]);
                {error, _Reason}=Error -> Error
            end;
        undefined when Default =/= undefined ->
            type_section(Types, RawSectionConfig, [{Field, Default}|Acc]);
        undefined ->
            {error, {missing_field, Field}}
    end.

type_field(_Type, RawValue) ->
    {ok, RawValue}.
