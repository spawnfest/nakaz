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
    {"can't execute 'magic function' that must be generated "
     "by nakaz_pt in module ~s", [Mod]};
r({missing, {Type, Value}}) ->
    {"~p ~s is missing in config", [Type, Value]};
r({invalid, {Name, Type, Value}}) ->
    {"value ~p for field ~p doesn't match ~s",
     [Value, Name, pp_type(Type)]};
r(UnknownError) ->
    ok = lager:warning("no clause for rendering error ~p", [UnknownError]),
    {"unrendered error: ~p", UnknownError}.

-spec pp_type(nakaz_typespec()) -> iolist().
pp_type({undefined, range, [From, To]}) ->
    io_lib:format("~p..~p", [From, To]);
pp_type({undefined, tuple, SubTypes}) ->
    io_lib:format("{~s}", [pp_types(SubTypes)]);
pp_type({undefined, union, SubTypes}) ->
    pp_types(SubTypes, " or ");
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
