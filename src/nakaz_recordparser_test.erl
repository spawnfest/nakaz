-module(nakaz_recordparser_test).

-behaviour(nakaz_user).

-compile({parse_transform, nakaz_recordparser}).
-export([somefunc/1]).
-export([nakaz_check/1, nakaz_load/1]).

-type mytype() :: integer().

-record(myrecord, {superfield :: integer()}).

-record(config, {
          my_any :: any(),
          my_int :: integer(),
          my_float :: float(),
          my_boolean :: boolean(),
	  my_binary :: binary(),
          my_atom :: atom(),
          my_range :: -199..199,
          my_non_neg_integer :: non_neg_integer(),
          my_pos_integer :: pos_integer(),
          my_neg_integer :: neg_integer(),
          my_number :: number(),
          my_string :: string(),
          my_nonempty_string :: nonempty_string(),
          my_module :: module(),
          my_node :: node(),
          my_timeout :: timeout(),
          my_none :: none(),
          my_byte :: byte(),
          my_char :: char(),
          my_empty_list :: [], % evaluates to nil
          my_nil :: nil(),
          my_typed_list :: [byte()],
          my_non_empty_list :: [any(),...],
          my_flist :: list(),
          my_nonempty_flist :: nonempty_list(),
          my_remote_type :: ?MODULE:mytype(),
          my_type :: mytype(),
          my_tuple :: tuple(),
          my_record :: #myrecord{}
         }).

-record(test, {foo :: atom()}).

-record(test1, {test :: #test{},
                interface :: inet:ip_address(),
                port :: pos_integer()}).

-record(test2, {interval :: non_neg_integer(),
                foo :: {1..10, string()}}).

-record(unions, {union :: a | b | integer()}).

somefunc(A) ->
    #test1{} = A,
    #test{} = A,
    #test2{} = A,
    #config{} = A,
    #myrecord{} = A,
    #unions{} = A.

nakaz_check(Conf) ->
    io:format("Got config to check: ~p~n", [Conf]),
    ok.

nakaz_load(Conf) ->
    io:format("Got config to load: ~p~n", [Conf]),
    ok.
