-module(nakaz_recordparser).
-export([parse_transform/2]).

-include("nakaz_internal.hrl").


parse_transform(Forms, _Options) ->
    Func = generate_schema_getter(Forms),
    FExport = generate_export(),
    %% We should insert export before any function definitions
    Forms2 = parse_trans:do_insert_forms(above, [FExport, Func], Forms, []),
%    io:format("Forms2 ~p~n", [Forms2]),
    Forms2.

generate_schema_getter(Forms) ->
    Schemas = extract_schemas(Forms),
    Func = erl_syntax:function(erl_syntax:atom(?NAKAZ_MAGIC_FUN),
                               [erl_syntax:clause(
                                  [],
                                  none,
                                  [erl_syntax:abstract({ok, Schemas})])]),
    erl_syntax:revert(Func).

%% FIXME: better export generation
generate_export() ->
    {attribute, 0, export, [{?NAKAZ_MAGIC_FUN, 0}]}.

extract_schemas(Forms) ->
    Module = parse_trans:get_module(Forms),
    lists:foldl(fun (F,Acc) -> handle_types(F,Acc,Module) end, [], Forms).

handle_types(Form, Acc, Module) ->
    case erl_syntax_lib:analyze_form(Form) of
        {attribute, {type, {type, Type}}} ->
            handle_record(Type, Acc, Module);
        _ ->
            Acc
    end.

%% Handle only records types
handle_record({{record, Name}, Fields, _Args}, Acc, Module) ->
    F = [handle_field(Field, Module) || Field <-Fields],
    [{Name, F} |Acc];
handle_record(_, Acc, _) ->
    Acc.

handle_field({typed_record_field, {record_field,_,{atom,_,Name}}, Type}, Module) ->
    Field = handle_field_type(Type, Module),
    {Name, Field, undefined};
%% Handle special case with default value
handle_field({typed_record_field, {record_field,_,{atom,_,Name}, Default}, Type}, Module) ->
    Field = handle_value_param(Type, Module),
    Def = handle_value_param(Default, Module),
    {Name, Field, Def}.

%%FIXME: Only allow typed fields
%%FIXME: Maybe there are different orders of 'undefined' atom
%%       and other term in union
handle_field_type({type,_,union,[{atom,_,undefined},
                                 Value]}, Module) ->
    handle_value_param(Value, Module);
handle_field_type(Other, _) ->
    {other, Other}.

handle_value_param({remote_type, LN, [{atom,_,Module},{atom,_,Type},Args]}, _Module) ->
    handle_value_param({type,LN, Type, Args}, Module);
handle_value_param({type, _, Type, TArgs}, Module) when is_list(TArgs) ->
    {Module, Type, [handle_value_param(TA, Module) || TA <- TArgs]};
handle_value_param({type, _, Type,  TArg}, Module) ->
    %% Handle special case when type arguments is not list
    {Module, Type, [TArg]};
handle_value_param({atom, _, Atom}, _) ->
    Atom;
handle_value_param({integer,_,Integer}, _) ->
    Integer;
handle_value_param({op,_, '-', {integer,_,Integer}}, _) ->
    %% Unary minus. Special case. Again.
    -Integer;
handle_value_param({float,_,Float}, _) ->
    Float;
handle_value_param({boolean,_,Boolean}, _) ->
    Boolean;
handle_value_param({tuple,_,Values}, Module) ->
    list_to_tuple([handle_value_param(V, Module) || V <- Values]);
handle_value_param(Other, Module) ->
    {Module, other, Other}.
