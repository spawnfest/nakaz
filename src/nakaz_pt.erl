-module(nakaz_pt).
-include("nakaz_internal.hrl").

-export([parse_transform/2]).


parse_transform(Forms, _Options) ->
    insert_specs_getter(Forms).

%% FIXME(Dmitry): spec
insert_specs_getter(Forms) ->
    Func = generate_specs_getter(Forms),
    FExport = generate_export(),
    %% We should insert export before any function definitions
    parse_trans:do_insert_forms(above, [FExport, Func], Forms, []).

%% FIXME(Dmitry): spec
generate_specs_getter(Forms) ->
    Module = parse_trans:get_module(Forms),
    Specs = nakaz_record_parser:extract_records_specs(Forms, Module),
    io:format("Specs ~p~n", [Specs]),
    Func = erl_syntax:function(erl_syntax:atom(?NAKAZ_MAGIC_FUN),
                               [erl_syntax:clause(
                                  [],
                                  none,
                                  [erl_syntax:abstract({ok, Specs})])]),
    erl_syntax:revert(Func).

%% FIXME: better export attribute generation
generate_export() ->
    {attribute, 0, export, [{?NAKAZ_MAGIC_FUN, 0}]}.
