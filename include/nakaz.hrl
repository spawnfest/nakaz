-define(NAKAZ_ENSURE(RECORDS),
        nakaz:ensure(?MODULE, RECORDS)).
-define(NAKAZ_ENSURE(RECORDS, OPTIONS),
        nakaz:ensure(?MODULE, RECORDS, OPTIONS)).
-define(NAKAZ_ENSURE(APP, RECORDS, OPTIONS),
        nakaz:ensure(?MODULE, APP, RECORDS, OPTIONS)).

-define(NAKAZ_USE(KEY),
        nakaz:use(?MODULE, KEY)).
%% FIXME(Dmitry): it's not a key, it's a section
-define(NAKAZ_USE(APP, KEY),
        nakaz:use(?MODULE, APP, KEY)).

-type nakaz_typespec() :: {Module :: module(),
                           Type   :: atom(),
                           Args   :: [any()]}.
