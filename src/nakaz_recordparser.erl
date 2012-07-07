-module(nakaz_recordparser).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    io:format("Forms:~p~n", [Forms]),
    Recs = lists:foldl(fun handle_types/2, [], Forms),
    io:format("Recs: ~p~n", [Recs]),
    Forms.

handle_types(Form, Acc) ->
    case erl_syntax_lib:analyze_form(Form) of
        {attribute, {type, {type, Type}}} ->
            handle_record(Type, Acc);
        _ ->
            Acc
    end.

%% Handle only records types
handle_record({{record, Name}, Fields, _Args}, Acc) ->
    F = [handle_field(Field) || Field <-Fields],
    [{record, Name, F} |Acc];
handle_record(_, Acc) ->
    Acc.

handle_field({typed_record_field, {record_field,_,{atom,_,Name}}, Type}) ->
    {T, TArgs} = handle_field_type(Type),
    {Name, T, TArgs}.

%%FIXME: Only allow typed fields
%%FIXME: Maybe there are different orders of 'undefined' atom
%%       and other term in union
handle_field_type({type,_,union,[{atom,_,undefined},
                                 Value]}) ->
    handle_value_param(Value);
handle_field_type(Other) ->
    {other, Other}.

handle_value_param({type, _, Type, TArgs}) when is_list(TArgs) ->
    {Type, [handle_value_param(TA) || TA <- TArgs]};
handle_value_param({type, _, Type, TArg}) ->
    %% Handle special case when type arguments is not list
    {Type, [TArg]};
handle_value_param({atom, _, Atom}) ->
    Atom;
handle_value_param({integer,_,Integer}) ->
    Integer;
handle_value_param({float,_,Float}) ->
    Float;
handle_value_param({boolean,_,Boolean}) ->
    Boolean;
handle_value_param(Other) ->
    {other, Other}.

