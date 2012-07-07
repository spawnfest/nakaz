-define(NAKAZ_USE(KEY), nakaz:use(?MODULE, KEY)).
-define(NAKAZ_USE(APP, KEY), nakaz:use(?MODULE, APP, KEY)).

-record(nakaz_field, {name   :: atom(),
                      value  :: {atom(), any()},
                      file   :: binary(),
                      line   :: pos_integer(),
                      column :: pos_integer()}).
