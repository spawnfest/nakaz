-module(nakaz_errors).
-compile([{parse_transform, lager_transform}]).

-export([render/1]).

render(Error) ->
    case r(Error) of
        {Msg, Args} ->
            iolist_to_binary(io_lib:format(Msg, Args));
        Msg when is_binary(Msg) ->
            Msg;
        Msg when is_list(Msg) ->
            list_to_binary(Msg)
    end.

r({cant_execute_magic_fun, Mod}) ->
    %% FIXME(Dmitry): rename parsetransform to nakaz_pt
    {"can't execute 'magic function' that must be generated "
     "by nakaz_pt in module ~s", [Mod]};
r({missing, {Type, Value}}) ->
    {"~p ~s is missing in config", [Type, Value]};
r({invalid, {Name, Type, Value}}) when is_binary(Value) ->
    {"value '~s' in field ~p doesn't match type ~s",
     [Value, Name, pp_type(Type)]};
r({no_entry_for_app, App}) ->
    {"no entry for app ~s in config", [App]};
r(UnknownError) ->
    lager:warning("no clause for rendering error ~p", [UnknownError]).

pp_type({undefined, Type, []}) ->
    atom_to_list(Type);
pp_type({undefined, range, [From, To]}) ->
    io_lib:format("~p..~p", [From, To]);
pp_type({undefined, Type, SubTypes}) ->
    io_lib:format("~s(~s)",
                  [Type, lists:join(lists:map(fun pp_type/1, SubTypes),
                                    ", ")]);
pp_type({Mod, Type, SubTypes}) ->
    io_lib:format("~s:~s", [Mod, pp_type({undefined, Type, SubTypes})]).
