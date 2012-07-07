-module(nakaz_loader).

-callback parse(Path :: [atom()],
                Type :: any(), %% FIXME(Dmitry)
                Value :: binary()) ->
    {ok, ParsedVal :: any()} |
    {error, Reason :: binary()}.

-callback validate(Path :: [atom()],
                   Type :: any(), %% FIXME(Dmitry)
                   Value :: any()) ->
    ok | {error, Reason :: binary()}.
