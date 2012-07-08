-module(nakaz_loader).

-callback parse(Type :: any(), %% FIXME(Dmitry)
                Value :: binary()) ->
    {ok, ParsedVal :: any()} |
    {error, Reason :: binary()}.

-callback validate(Type :: any(), %% FIXME(Dmitry)
                   Value :: any()) ->
    ok | {error, Reason :: binary()}.
