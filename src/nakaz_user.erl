-module(nakaz_user).

%% FIXME(Dmitry): enforce use of this behaviour if one uses nakaz:use(...)

%% FIXME(Dmitry): comment
-callback nakaz_check(Config :: any()) ->
    ok | {error, Reason :: binary()}.

%% FIXME(Dmitry): comment
-callback nakaz_load(Config :: any()) ->
    ok | {error, Reason :: binary()}.
