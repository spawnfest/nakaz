-type reload_type() :: sync | async.

-type ret_novalue() :: ok
                     | {error, Reason :: binary()}.

-type ret_value(T) :: {ok, T}
                    | {error, Reason :: binary()}.
