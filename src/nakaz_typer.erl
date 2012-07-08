%%% @author Sergei Levedev <superbobry@gmail.com>
%%%
%%% @doc
%%% This module implements a {@see type/3} function, which transforms
%%% a raw section config, consisting of <em>only</em> atoms and binaries
%%% into a record, defined by a given `RecordSpec'. Keep in mind that
%%% {@see type/3} only works on <b>section</b> level:
%%%
%%% ```
%%% my_loger:                           # application level
%%%   file_backend:                     # section level
%%%     path: "/var/log/my_logger.log"
%%%     rotate_after: 1M
%%%
%%% my_http_server:
%%%   listener: {port: 4242}
%%% '''
%%%
%%% So, in the example above, `my_loger' application has a single section,
%%% named `file_backend', which must have an <em>ensured</em> record
%%% somewhere in the application source code:
%%%
%%% ```
%%% -module(my_loger).
%%% -behaviour(application).
%%%
%%% -record(file_backend, {path :: binary(), rotate_after: binary()}).
%%%
%%% %% somewhere over the rainbow
%%%   nakaz:ensure(?MODULE, [#file_backend{}]).
%%% '''
%%% @end
%%%

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

type_field({undefined, Atom, []}, {RawValue, _Pos})
  when is_atom(RawValue) andalso
       (Atom =:= atom orelse Atom =:= node orelse Atom =:= module) ->
    %% Note(Sergei): atoms is the only 'special' case, since we do the
    %% conversion in 'nakaz_composer:compose_mapping'.
    {ok, RawValue};
type_field(Type, {RawValue, _Pos}) when is_atom(RawValue) ->
    {error, {invalid, Type, atom_to_binary(RawValue, utf8)}};
type_field({undefined, Atom, []}=Type, {RawValue, _Pos})
  when Atom =:= atom orelse
       Atom =:= node orelse
       Atom =:= module ->
    try binary_to_atom(RawValue, utf8) of
        Value -> {ok, Value}
    catch
        error:badarg -> {error, {invalid, Type, RawValue}}
    end;
type_field({undefined, binary, []}, {RawValue, _Pos}) ->
    {ok, RawValue};
type_field({undefined, String, []}=Type, {RawValue, _Pos})
  when String =:= string orelse String =:= nonempty_string ->
    case binary_to_list(RawValue) of
        [] when String =:= nonempty_string ->
            {error, {invalid, Type, RawValue}};
        Value -> {ok, Value}
    end;
type_field({undefined, Integer, []}=Type, {RawValue, _Pos})
  when Integer =:= integer orelse
       Integer =:= pos_integer orelse
       Integer =:= neg_integer orelse
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
                neg_integer when Value < 0 -> {ok, Value};
                non_neg_integer when Value >= 0 -> {ok, Value};
                integer -> {ok, Value};
                _       -> {error, {invalid, Type, RawValue}}
            end
    catch
        error:badarg -> {error, {invalid, Type, RawValue}}
    end;
type_field({undefined, range, [From, To]}=Type, {RawValue, Pos}) ->
    case type_field({undefined, integer, []}, {RawValue, Pos}) of
        {ok, Value} when Value >= From andalso Value =< To -> {ok, Value};
        {ok, _Value} -> {error, {invalid, Type, RawValue}};
        {error, _Reason}=Error -> Error
    end;
type_field({undefined, float, []}=Type, {RawValue, _Pos}) ->
    case string:to_float(binary_to_list(RawValue)) of
        {Value, []} -> {ok, Value};
        _           -> {error, {invalid, Type, RawValue}}
    end;
type_field({undefined, number, []}=Type, {RawValue, Pos}) ->
    case type_union([{undefined, integer, []},
                     {undefined, float, []}], {RawValue, Pos}) of
        {ok, {_, Value}} -> {ok, Value};
        _Any -> {error, {invalid, Type, RawValue}}
    end;
type_field({undefined, tuple, Types}, {RawValues, _Pos}) ->
    case type_composite(Types, RawValues) of
        {ok, Values} -> {ok, list_to_tuple(Values)};
        {error, _Reason}=Error -> Error
    end;
type_field({undefined, record, [Name]}, {RawValues, Pos})
  when is_list(RawValues) ->
    RecordSpecs = get(record_specs),
    %% FIXME(Sergei): check if this records has a spec!
    {Name, RecordSpec} = lists:keyfind(Name, 1, RecordSpecs),
    case type_section(RecordSpec, {RawValues, Pos}) of
        {ok, TypedSectionConfig} ->
            {ok, section_to_record(Name, RecordSpec, TypedSectionConfig)};
        {error, _Reason}=Error -> Error
    end;
type_field({undefined, List, [SubType]}=Type, {RawValues, _Pos})
  when List =:= list orelse List =:= nonempty_list ->
    case type_composite([SubType || _ <- RawValues], RawValues) of
        {ok, []} when List =:= nonempty_list ->
            {error, {invalid, Type, RawValues}};
        {ok, Values} -> {ok, Values};
        {error, _Reason}=Error -> Error
    end;
type_field({undefined, timeout, []}=Type, {RawValue, Pos}) ->
    Atom = {undefined, atom, []},
    NonNegInteger = {undefined, non_neg_integer, []},
    case type_union([Atom, NonNegInteger], {RawValue, Pos}) of
        {ok, {Atom, infinity}} -> {ok, infinity};
        {ok, {NonNegInteger, Value}} -> {ok, Value};
        _Any -> {error, {invalid, Type, RawValue}}
    end;
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

type_union([Type|Types], {RawValue, Pos}) ->
    case type_field(Type, {RawValue, Pos}) of
        {ok, Value} -> {ok, {Type, Value}};
        {error, _Reason} when Types =/= [] ->
            type_union(Types, {RawValue, Pos});
        {error, _Reason}=Error -> Error
    end.

type_composite(Types, RawValues) ->
    type_composite(Types, RawValues, []).

type_composite([Type|Types], [RawValue|RawValues], Acc) ->
    case type_field(Type, RawValue) of
        {ok, Value} -> type_composite(RawValues, Types,
                                      [Value|Acc]);
        {error, _Reason}=Error -> Error
    end;
type_composite([], [], Acc) ->
    {ok, lists:reverse(Acc)};
type_composite(_Types, [], _Acc) ->
    %% FIXME(Sergei): sane error message?
    {error, {invalid, not_sure_what_to_report, <<>>}};
type_composite([], _RawValues, _Acc) ->
    %% FIXME(Sergei): sane error message?
    {error, {invalid, not_sure_what_to_report, <<>>}}.

section_to_record(Section, RecordSpec, TypedSectionConfig) ->
    %% FIXME(Sergei): hopefully field order is correct.
    Fields = [proplists:get_value(Field, TypedSectionConfig)
              || {Field, _Type, _Default} <- RecordSpec],
    list_to_tuple([Section|Fields]).
