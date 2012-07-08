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
        {RawValue, _FieldPos} ->
            case type_field(Type, RawValue) of
                {ok, TypedValue} ->
                    type_section(RecordSpec,
                                 {RawSectionConfig, Pos},
                                 [{Field, TypedValue}|Acc]);
                {error, _Reason}=Error -> Error
            end;
        undefined when Default =/= undefined ->
            type_section(RecordSpec,
                         {RawSectionConfig, Pos},
                         [{Field, Default}|Acc]);
        undefined ->
            {error, {missing_field, Field}}
    end.

type_field({_Mod, atom, []}, RawValue) when is_atom(RawValue) ->
    {ok, RawValue};
type_field({_Mod, integer, []}, RawValue) when is_integer(RawValue) ->
    {ok, RawValue};
type_field({_Mod, pos_integer, []}, RawValue)
  when is_integer(RawValue) and RawValue > 0 ->
    {ok, RawValue};
type_field({_Mod, non_neg_integer, []}, RawValue)
  when is_integer(RawValue) and RawValue >= 0 ->
    {ok, RawValue};
type_field({_Mod, float, []}, RawValue) when is_float(RawValue) ->
    {ok, RawValue};

type_field({_Mod, atom, []}=Type, <<RawValue/binary>>) ->
    type_field(Type, binary_to_atom(RawValue, utf8));
type_field({_Mod, Integer, []}=Type, <<RawValue/binary>>)
  when Integer =:= integer orelse
       Integer =:= pos_integer orelse
       Integer =:= non_neg_integer ->
    type_field(Type, list_to_integer(binary_to_list(RawValue)));
type_field({_Mod, list, [Type]}, RawValues) when is_list(RawValues) ->
    lists:foldl(
      fun ({RawValue, _Pos}, Acc) ->
              case Acc of
                  {error, _Reason}=Error -> Error;
                  {ok, Values}     ->
                      %% FIXME(Sergei): force monomorphism?
                      case type_field(Type, RawValue) of
                          {ok, Value} -> {ok, [Value|Values]};
                          {error, _Reason}=Error -> Error
                      end
              end
      end, {ok, []}, RawValues);
type_field({inet, ip_address, []}, <<RawValue/binary>>) ->
    inet_parse:address(binary_to_list(RawValue));
type_field({inet, ip4_address, []}, <<RawValue/binary>>) ->
    inet_parse:ipv4_address(binary_to_list(RawValue));
type_field({inet, ip6_address, []}, <<RawValue/binary>>) ->
    inet_parse:ipv6_address(binary_to_list(RawValue));
type_field(_Type, RawValue) ->
    %% FIXME(Sergei): catch type mismatches here!
    {ok, RawValue}.

section_to_record(Section, RecordSpec, TypedSectionConfig) ->
    %% FIXME(Sergei): hopefully field order is correct.
    Fields = [proplists:get_value(Field, TypedSectionConfig)
              || {Field, _Type, _Default} <- RecordSpec],
    list_to_tuple([Section|Fields]).
