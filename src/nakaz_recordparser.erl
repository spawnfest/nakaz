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
            handle_type(Type, Acc);
        _ ->
            Acc
    end.

handle_type({{record, Name}, Tree, _Args}, Acc) ->
    [{record, Name, Tree}|Acc];
handle_type(_, Acc) ->
    Acc.
