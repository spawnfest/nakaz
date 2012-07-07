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
r({missing_section, Section}) ->
    {"section ~s is missing in config", [Section]};
r({no_entry_for_app, App}) ->
    {"no entry for app ~s in config", [App]};
r(UnknownError) ->
    lager:warning("No clause for rendering error ~p", [UnknownError]).
