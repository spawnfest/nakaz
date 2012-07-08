-module(nakaz_parse_trans).

%% FIXME(Dmitry): WTF?! import is pure shit
-import(nakaz_recordparser, [insert_specs_getter/2]).
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    insert_specs_getter(Forms, [config]).
