-module(nakaz_recordparser_test).
-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, nakaz_recordparser}).

-type loltype() :: integer().

-record(config1, {
          i :: integer() | lol,
          a :: some_atom,
          lold :: 1..100,
          v = 10
         }).

simple_types_test() ->
    ?assertEqual(0,0).
