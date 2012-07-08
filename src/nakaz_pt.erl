%%% @author Alexander Neganov <ikkeps@gmail.com>
%%%
%%% @doc
%%% The current implementation simply parses all records in the transformed
%%% module, and inserts parsed typespecs in a magic function, which can
%%% then be callsed by {@see nakaz_core} internals to get specs for any
%%% particular record.
%%% @end

-module(nakaz_pt).

-include("nakaz_internal.hrl").

%% API

-export([parse_transform/2]).

%% API

parse_transform(Forms, _Options) ->
    Fun    = generate_magic_fun(Forms),
    Export = generate_export(),

    %% Note(Sasha): we should insert export before any other function
    %% definitions.
    parse_trans:do_insert_forms(above, [Export, Fun], Forms, []).

%% Internal

generate_magic_fun(Forms) ->
    Module = parse_trans:get_module(Forms),
    Specs  = nakaz_record_parser:extract_records_specs(Forms, Module),
    FunDef = erl_syntax:function(erl_syntax:atom(?NAKAZ_MAGIC_FUN),
                                 [erl_syntax:clause(
                                    [],
                                    none,
                                    [erl_syntax:abstract({ok, Specs})])]),
    erl_syntax:revert(FunDef).

%% @doc Generate 'export' module attribute for magick function
%% FIXME: better export attribute generation
generate_export() ->
    {attribute, 0, export, [{?NAKAZ_MAGIC_FUN, 0}]}.
