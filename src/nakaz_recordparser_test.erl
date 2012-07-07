-module(nakaz_recordparser_test).
-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, nakaz_recordparser}).

-type mytype() :: integer().

-record(myrecord, {superfield :: integer()}).

-record(config1, {
          my_any :: any(),
          my_int :: integer(),
          my_float :: float(),
          my_boolean :: boolean(),
          my_atom :: atom(),
          my_range :: 1..10,
          my_non_neg_integer :: non_neg_integer(),
          my_pos_integer :: pos_integer(),
          my_neg_integer :: neg_integer(),
          my_number :: number(),
          my_string :: string(),
          my_nonempty_string :: nonempty_string(),
          my_module :: module(),
          %my_mfa :: mfa(),
          my_node :: node(),
          my_timeout :: timeout(),
          my_none :: none(),
          my_pid :: pid(),
          my_byte :: byte(),
          my_char :: char(),
          my_port :: port(),
          my_ref :: reference(),
          my_empty_list :: [], % evaluates to nil
          my_nil :: nil(),
          my_typed_list :: [reference()],
          my_non_empty_list :: [any(),...],
          my_flist :: list(),
          my_nonempty_flist :: nonempty_list(),
          my_type :: mytype(),
          my_tuple :: tuple(),
          my_record :: #myrecord{},
          my_record2 :: record()
         }).

simple_types_test() ->
    ?assertEqual(#config1{}, #config1{}).
