-module(nakaz_loader).

%% FIXME(Dmitry): comment
-callback parse(Type :: any(), %% FIXME(Dmitry)
                Value :: binary()) ->
    {ok, ParsedVal :: any()} |
    {error, Reason :: binary()}.

%% FIXME(Dmitry): comment
-callback validate(Type :: any(), %% FIXME(Dmitry)
                   Value :: any()) ->
    ok | {error, Reason :: binary()}.
