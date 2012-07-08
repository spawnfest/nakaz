-module(nakaz_errors).
-compile({parse_transform, lager_transform}).

-include("nakaz_internal.hrl").

-export([render/1]).

-spec render(any()) -> binary().
render(Error) ->
    {Msg, Args} = r(Error),
    iolist_to_binary(io_lib:format(Msg, Args)).

-spec r(any()) -> {string(), [any()]}.
r({cant_execute_magic_fun, Mod}) ->
    %% FIXME(Dmitry): rename parsetransform to nakaz_pt
    {"Can't execute 'magic function' that must be generated "
     "by nakaz_pt in module ~s", [Mod]};
r({malformed, [{app, {Name, _Body}}|_Rest]}) ->
    {"Malformed application structure in ~p, sections aren't mappings?",
     [Name]};
r({malformed, [{section, {Name, _Body}}|_Rest]}) ->
    {"Malformed section structure in ~p, not a mapping?'",
     [Name]};
r({missing, {app, Name}}) ->
    {"Missing application ~p", [Name]};
r({missing, {section, Name, App}}) ->
    {"Missing section ~p for application ~p", [Name, App]};
r({missing, {field, Name, Section}}) ->
    {"Missing field ~p in section ~p", [Name, Section]};
r({invalid, {Name, Type, Value, {Line, _Column}}}) ->
    {"Invalid type at line ~p: value ~p for field ~p doesn't match ~s",
     [Line, Value, Name, pp_type(Type)]};
r({unsupported, Line, Mod}) ->
    {"Unsupported type expression at ~p.erl:~p", [Mod, Line]};
r(UnknownError) ->
    io:format("~p~n", [UnknownError]),
    ok = lager:warning("no clause for rendering error ~p", [UnknownError]),
    {"Evil martians are remote controlling your node! maybe that'll help: ~p",
     [UnknownError]}.

-spec pp_type(nakaz_typespec()) -> iolist().
pp_type({undefined, range, [From, To]}) ->
    io_lib:format("~p..~p", [From, To]);
pp_type({undefined, tuple, SubTypes}) ->
    io_lib:format("{~s}", [pp_types(SubTypes)]);
pp_type({undefined, union, SubTypes}) ->
    Sep = " or ",
    case lists:all(fun is_atom/1, SubTypes) of
        true  ->
            string:join(lists:map(fun atom_to_list/1, SubTypes), Sep);
        false -> pp_types(SubTypes, Sep)
    end;
pp_type({undefined, Type, []}) ->
    atom_to_list(Type);
pp_type({undefined, Type, SubTypes}) ->
    io_lib:format("~s(~s)", [Type, pp_types(SubTypes)]);
pp_type({Mod, Type, SubTypes}) ->
    io_lib:format("~s:~s", [Mod, pp_type({undefined, Type, SubTypes})]).

-spec pp_types([nakaz_typespec()]) -> string().
pp_types(Types) ->
    pp_types(Types, ", ").

-spec pp_types([nakaz_typespec()], string()) -> string().
pp_types(Types, Sep) ->
    string:join(lists:map(fun pp_type/1, Types), Sep).
