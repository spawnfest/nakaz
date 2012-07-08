%%% @doc
%%% For now, these module just parses module records specs and inserts in it
%%% some magic function, that returns parsed records specs. NAKAZ_ENSURE
%%% and NAKAZ_USE uses this parsed record specs.
%%% @end
-module(nakaz_pt).
-include("nakaz_internal.hrl").

-export([parse_transform/2]).


parse_transform(Forms, _Options) ->
    %% TODO: check that module haz special behaviour
    %% TODO: check that required record specs is valid at compile time
    insert_specs_getter(Forms).

%% @doc Inserts magic function that returns record specs into forms.
-spec insert_specs_getter(Forms::term()) -> NewForms::term().
insert_specs_getter(Forms) ->
    Func = generate_specs_getter(Forms),
    FExport = generate_export(),
    %% We should insert export before any function definitions
    parse_trans:do_insert_forms(above, [FExport, Func], Forms, []).

%% @doc Generate record specs getter function
-spec generate_specs_getter(Forms::term()) -> FuncForm::term().
generate_specs_getter(Forms) ->
    Module = parse_trans:get_module(Forms),
    Specs = nakaz_record_parser:extract_records_specs(Forms, Module),
    Func = erl_syntax:function(erl_syntax:atom(?NAKAZ_MAGIC_FUN),
                               [erl_syntax:clause(
                                  [],
                                  none,
                                  [erl_syntax:abstract({ok, Specs})])]),
    erl_syntax:revert(Func).

%% @doc Generate 'export' module attribute for magick function
%% FIXME: better export attribute generation
generate_export() ->
    {attribute, 0, export, [{?NAKAZ_MAGIC_FUN, 0}]}.
