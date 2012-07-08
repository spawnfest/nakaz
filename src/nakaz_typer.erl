-module(nakaz_typer).
-include("nakaz_internal.hrl").

%% API

-export([type/3]).

%% API

-spec type(atom(), raw_config(), record_specs())
          -> {ok, record()} | {error, typer_error()}.
type(Section, RawSectionConfig, RecordSpecs) ->
    {Section, RecordSpec} = lists:keyfind(Section, 1, RecordSpecs),
    put(record_specs, RecordSpecs),
    Result =
        case type_section(RecordSpec, RawSectionConfig) of
            {ok, TypedSectionConfig} ->
                {ok, section_to_record(Section, RecordSpec, TypedSectionConfig)};
            {error, _Reason}=Error -> Error
        end,
    erase(record_specs),
    Result.

%% Internal

type_section(RecordSpec, RawSectionConfig) ->
    type_section(RecordSpec, RawSectionConfig, []).

type_section([], {_RawSectionConfig, _Pos}, Acc) ->
    {ok, lists:reverse(Acc)};
type_section([{Field, Type, Default}|RecordSpec],
             {RawSectionConfig, Pos}, Acc) ->
    case proplists:get_value(Field, RawSectionConfig) of
        {_, _}=RawValueWithPos ->
            case type_field(Type, RawValueWithPos) of
                {ok, TypedValue} ->
                    type_section(RecordSpec,
                                 {RawSectionConfig, Pos},
                                 [{Field, TypedValue}|Acc]);
                {error, {invalid, InferedType, RawValue}} ->
                    %% FIXME(Sergei): report field position!
                    {error, {invalid, {Field, InferedType, RawValue}}}
            end;
        undefined when Default =/= undefined ->
            type_section(RecordSpec,
                         {RawSectionConfig, Pos},
                         [{Field, Default}|Acc]);
        undefined ->
            {error, {missing, {field, Field}}}
    end.

type_field({_Mod, atom, []}, {RawValue, _Pos}) when is_atom(RawValue) ->
    %% Note(Sergei): atoms is the only 'special' case, since we do the
    %% conversion in 'nakaz_composer:compose_mapping'.
    {ok, RawValue};
type_field({_Mod, atom, []}, {RawValue, _Pos}) ->
    try binary_to_atom(RawValue, utf8) of
        Value -> {ok, Value}
    catch
        error:badarg -> {error, {invalid, atom, RawValue}}
    end;
type_field({_Mod, binary, []}, {RawValue, _Pos}) ->
    {ok, RawValue};
type_field({_Mod, Integer, []}, {RawValue, _Pos})
  when Integer =:= integer orelse
       Integer =:= pos_integer orelse
       Integer =:= non_neg_integer ->
    {Base, RawPart} = case RawValue of
                          <<"0x", Part/binary>> -> {16, Part};
                          <<"0o", Part/binary>> -> {8,  Part};
                          Part                  -> {10, Part}
                      end,
    try list_to_integer(binary_to_list(RawPart), Base) of
        Value ->
            case Integer of
                pos_integer when Value > 0 -> {ok, Value};
                non_neg_integer when Value >= 0 -> {ok, Value};
                integer -> {ok, Value};
                _       -> {error, {invalid, Integer, RawValue}}
            end
    catch
        error:badarg -> {error, {invalid, Integer, RawValue}}
    end;
type_field({_Mod, float, []}, {RawValue, _Pos}) ->
    case string:to_float(binary_to_list(RawValue)) of
        {Value, []} -> {ok, Value};
        _           -> {error, {invalid, float, RawValue}}
    end;
type_field({_Mod, tuple, Types}, {RawValues, _Pos}) ->
    case type_composite(RawValues, Types) of
        {ok, Values} -> {ok, list_to_tuple(Values)};
        {error, _Reason}=Error -> Error
    end;
type_field({_Mod, record, [Name]}, {RawValues, Pos}) when is_list(RawValues) ->
    RecordSpecs = get(record_specs),
    %% FIXME(Sergei): check if this records has a spec!
    {Name, RecordSpec} = lists:keyfind(Name, 1, RecordSpecs),
    case type_section(RecordSpec, {RawValues, Pos}) of
        {ok, TypedSectionConfig} ->
            {ok, section_to_record(Name, RecordSpec, TypedSectionConfig)};
        {error, _Reason}=Error -> Error
    end;
type_field({_Mod, list, [Type]}, {RawValues, _Pos}) when is_list(RawValues) ->
    %% FIXME(Sergei): lists are currently monomorphic.
    type_composite(RawValues, [Type || _ <- RawValues]);
type_field({inet, ip_address, []}, {RawValue, _Pos}) ->
    inet_parse:address(binary_to_list(RawValue));
type_field({inet, ip4_address, []}, {RawValue, _Pos}) ->
    inet_parse:ipv4_address(binary_to_list(RawValue));
type_field({inet, ip6_address, []}, {RawValue, _Pos}) ->
    inet_parse:ipv6_address(binary_to_list(RawValue));
type_field(_Type, RawValue) ->
    %% FIXME(Sergei): catch type mismatches here!
    io:format(">>> type = ~p, value = ~p ~n", [_Type, RawValue]),
    {ok, RawValue}.

type_composite(RawValues, Types) ->
    type_composite(RawValues, Types, []).

type_composite([RawValue|RawValues], [Type|Types], Acc) ->
    case type_field(Type, RawValue) of
        {ok, Value} -> type_composite(RawValues, Types,
                                      [Value|Acc]);
        {error, _Reason}=Error -> Error
    end;
type_composite([], [], Acc) ->
    {ok, lists:reverse(Acc)};
type_composite(_RawValues, [], _Acc) ->
    %% FIXME(Sergei): sane error message?
    {error, {invalid, not_sure_what_to_report, <<>>}};
type_composite([], _Types, _Acc) ->
    %% FIXME(Sergei): sane error message?
    {error, {invalid, not_sure_what_to_report, <<>>}}.

section_to_record(Section, RecordSpec, TypedSectionConfig) ->
    %% FIXME(Sergei): hopefully field order is correct.
    Fields = [proplists:get_value(Field, TypedSectionConfig)
              || {Field, _Type, _Default} <- RecordSpec],
    list_to_tuple([Section|Fields]).
