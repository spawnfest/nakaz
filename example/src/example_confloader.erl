-module(example_confloader).
-behaviour(nakaz_loader).
-export([parse/2, validate/2]).

parse({example_app, filename, []}, Path) ->
    {ok, binary_to_list(Path)}.

validate({example_app, filename, []}, Path) ->
    case filelib:last_modified(Path) of
        T when T > 0 -> ok;
        _ -> {error, <<"file doesn't exist">>}
    end.
