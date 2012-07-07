
-type raw_position() :: {Line   :: non_neg_integer(),
                         Column :: non_neg_integer()}.

-type raw_field() :: {Name  :: atom(),
                      Value :: term(),
                      raw_position()}.

-type raw_config() :: [raw_field()].

-type typer_error() :: {missing_section, atom()}
                     | {missing_field, atom()}.

-type composer_error() :: any().

-type reload_type() :: sync | async.

-type ret_novalue() :: ok
                     | {error, Reason :: binary()}.

-type ret_value(T) :: {ok, T}
                    | {error, Reason :: binary()}.
