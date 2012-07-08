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

-export([type/3, type/4]).

%% API

-spec type(atom(), raw_config(), record_specs())
          -> {ok, record_()} | {error, typer_error()}.
type(Section, RawSectionConfig, RecordSpecs) ->
    type(Section, RawSectionConfig, RecordSpecs, undefined).

-spec type(atom(), raw_config(), record_specs(), undefined | module())
          -> {ok, record_()} | {error, typer_error()}.
type(Section, RawSectionConfig, RecordSpecs, Mod) ->
    {Section, RecordSpec} = lists:keyfind(Section, 1, RecordSpecs),
    put(loader, Mod),
    put(record_specs, RecordSpecs),
    Result =
        case type_section(RecordSpec, RawSectionConfig) of
            {ok, TypedSectionConfig} ->
                {ok, section_to_record(Section, RecordSpec, TypedSectionConfig)};
            {error, _Reason}=Error -> Error
        end,
    erase(loader),
    erase(record_specs),
    Result.

%% Internal

-spec type_section(nakaz_typespec(), raw_config())
                  -> {ok, typed_config()} | {error, typer_error()}.
type_section(RecordSpec, RawSectionConfig) ->
    type_section(RecordSpec, RawSectionConfig, []).

type_section([], {_RawSectionConfig, _Pos}, Acc) ->
    {ok, lists:reverse(Acc)};
type_section([{Field, Type, Default}|RecordSpec],
             {RawSectionConfig, Pos}, Acc) ->
    case proplists:get_value(Field, RawSectionConfig) of
        {_, _}=RawValueWithPos ->
            ResolvedType = resolve_type_synonym(Type),
            case type_field(ResolvedType, RawValueWithPos) of
                {ok, TypedValue} ->
                    type_section(RecordSpec,
                                 {RawSectionConfig, Pos},
                                 [{Field, TypedValue}|Acc]);
                {error, {Reason, InferedType, RawValue}} ->
                    %% FIXME(Sergei): report field position!
                    {error, {Reason, {Field, InferedType, RawValue}}}

            end;
        undefined when Default =/= undefined ->
            type_section(RecordSpec,
                         {RawSectionConfig, Pos},
                         [{Field, Default}|Acc]);
        undefined ->
            {error, {missing, {field, Field}}}
    end.

-spec type_field(nakaz_typespec(), raw_field())
                -> {ok, typed_term()}
                 | {error, {atom(), atom(), binary()}}.
type_field({undefined, Atom, []}, {RawValue, _Pos})
  when is_atom(RawValue) andalso
       (Atom =:= atom orelse Atom =:= node orelse Atom =:= module) ->
    %% Note(Sergei): atoms is the only 'special' case, since we do the
    %% conversion in 'nakaz_composer:compose_mapping'.
    {ok, RawValue};
type_field(Type, {RawValue, _Pos}) when is_atom(RawValue) ->
    {error, {invalid, Type, atom_to_binary(RawValue, utf8)}};
type_field({undefined, Atom, Types}=Type, {RawValue, _Pos})
  when Atom =:= atom orelse
       Atom =:= node orelse
       Atom =:= module ->
    try binary_to_atom(RawValue, utf8) of
        Value ->
            %% Note(Sergei): the exact value of an 'atom()' might be
            %% restricted by a single type parameter.
            case Types of
                []      -> {ok, Value};
                [Value] -> {ok, Value};
                [_|_]   -> {error, {invalid, Type, RawValue}}
            end
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
type_field({undefined, union, Types}=Type, {RawValue, Pos}) ->
    %% Note(Sergei): a special case for a union of atoms, like
    %% `foo | bar | baz'.
    ActualTypes =
        case lists:all(fun is_atom/1, Types) of
            true  -> [{undefined, atom, [Atom]} || Atom <- Types];
            false -> Types
        end,
    case type_union(Type, ActualTypes, {RawValue, Pos}) of
        {ok, {_Type, Value}}   -> {ok, Value};
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
    case type_union(Type,
                    [{undefined, atom, [infinity]},
                     {undefined, non_neg_integer, []}],
                    {RawValue, Pos}) of
        {ok, {_Type, Value}}   -> {ok, Value};
        {error, _Reason}=Error -> Error
    end;
type_field({inet, IpAddress, []}=Type, {RawValue, _Pos})
  when IpAddress =:= ip_address orelse
       IpAddress =:= ip4_address orelse
       IpAddress =:= ip6_address ->
    Parser = case IpAddress of
                 ip_address  -> address;
                 ip4_address -> ipv4_address;
                 ip6_address -> ipv6_address
             end,
    case inet_parse:Parser(binary_to_list(RawValue)) of
        {ok, Value}      -> {ok, Value};
        {error, _Reason} -> {error, {invalid, Type, RawValue}}
    end;
type_field(Type, {RawValue, _Pos}) ->
    %% Okay, we're out of luck, 'nakaz_typer' doesn't support this
    %% type, our last hope is user-defined 'loader_module'.
    case get(loader) of
        undefined ->
            %% ... last hope lost -- no 'loader_module' defined.
            {error, {unknown, Type, RawValue}};
        Mod ->
            %% ... okay, we have a module, first transform the value
            %% and then validate it, note that both functions might
            %% *NOT* have a clause for the type being processed, in
            %% that case no transformation nor validation will be
            %% done.
            try Mod:parse(Type, RawValue) of
                {ok, Value} ->
                    try Mod:validate(Type, Value) of
                        ok -> {ok, Value};
                        {error, Reason} ->
                            {error, Reason, {Type, RawValue}}
                    catch
                        error:case_clause -> {ok, Value}
                    end;
                {error, Reason} ->
                    {error, {Reason, Type, RawValue}}
            catch
                error:case_clause ->
                    {error, {unknown, Type, RawValue}}
            end
    end.

-spec type_union(nakaz_typespec(), [nakaz_typespec()], raw_field())
                -> {ok, typed_field()}
                 | {error, {atom(), atom(), binary()}}.
type_union(OriginalType, [], {RawValue, _Pos})->
    {error, {invalid, OriginalType, RawValue}};
type_union(OriginalType, [Type|Types], {RawValue, Pos}) ->
    case type_field(Type, {RawValue, Pos}) of
        {ok, Value} -> {ok, {Type, Value}};
        {error, _Reason} ->
            type_union(OriginalType, Types, {RawValue, Pos})
    end.

-spec type_composite([nakaz_typespec()], [raw_field()])
                    -> {ok, [typed_term()]}
                     | {error, {atom(), atom(), binary()}}.
type_composite(Types, RawValues) ->
    type_composite(Types, RawValues, []).

type_composite([Type|Types], [RawValue|RawValues], Acc) ->
    case type_field(Type, RawValue) of
        {ok, Value} -> type_composite(Types, RawValues,
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

%% FIXME(Sergei): find a builtin function, which does the same thing?
-spec resolve_type_synonym(nakaz_typespec()) -> nakaz_typespec().
resolve_type_synonym({undefined, byte, []}) -> {undefiend, range, [0, 16#ff]};
resolve_type_synonym({undefined, char, []}) -> {undefined, range, [0, 16#10fff]};
resolve_type_synonym({undefined, number, []}) ->
    {undefined, union, [{undefined, integer, []}, {undefined, float, []}]};
resolve_type_synonym({undefined, list, [Type]}) ->
    {undefined, tuple, [resolve_type_synonym(Type)]};
resolve_type_synonym({undefined, Composite, Types})
  when Composite =:= tuple orelse Composite =:= union ->
    {undefined, Composite, lists:map(fun resolve_type_synonym/1, Types)};
resolve_type_synonym(Type) -> Type.

-spec section_to_record(atom(),
                        record_spec(),
                        typed_config()) -> record_().
section_to_record(Section, RecordSpec, TypedSectionConfig)
  when is_atom(Section) ->
    %% FIXME(Sergei): hopefully field order is correct.
    Fields = [proplists:get_value(Field, TypedSectionConfig)
              || {Field, _Type, _Default} <- RecordSpec],
    list_to_tuple([Section|Fields]).
