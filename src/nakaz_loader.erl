-module(nakaz_loader).

-callback parse(Path :: [atom()], Value :: binary()) ->
    {ok, ParsedVal :: any()} |
    {error, Reason :: binary()}.

-callback validate(Path :: [atom()], Value :: any()) ->
    ok | {error, Reason :: binary()}.
