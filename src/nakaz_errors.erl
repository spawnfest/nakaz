-module(nakaz_errors).
-compile([{parse_transform, lager_transform}]).

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
    {"value ~p in field ~p doesn't match type ~s",
     [Value, Name, pp_type(Type)]};
r(UnknownError) ->
    ok = lager:warning("no clause for rendering error ~p", [UnknownError]),
    {"unrendered error: ~p", UnknownError}.

pp_type({undefined, range, [From, To]}) ->
    io_lib:format("~p..~p", [From, To]);
pp_type({undefined, tuple, SubTypes}) ->
    io_lib:format(
      "{~s}", [string:join(lists:map(fun pp_type/1, SubTypes),
                           ", ")]);
pp_type({undefined, Type, []}) ->
    atom_to_list(Type);
pp_type({undefined, Type, SubTypes}) ->
    io_lib:format("~s(~s)",
                  [Type, string:join(lists:map(fun pp_type/1, SubTypes),
                                     ", ")]);
pp_type({Mod, Type, SubTypes}) ->
    io_lib:format("~s:~s", [Mod, pp_type({undefined, Type, SubTypes})]).
