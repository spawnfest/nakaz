-module(nakaz_user).

%% FIXME(Dmitry): enforce use of this behaviour if one uses nakaz:use(...)

-callback nakaz_check(Config :: any()) ->
    ok | {error, Reason :: binary()}.

-callback nakaz_load(Config :: any()) ->
    ok | {error, Reason :: binary()}.
