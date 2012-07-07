-module(nakaz_user).

-callback nakaz_check(Config :: any()) ->
    ok | {error, Reason :: binary()}.

-callback nakaz_load(Config :: any()) ->
    ok | {error, Reason :: binary()}.
