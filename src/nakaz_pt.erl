-module(nakaz_pt).

-export([parse_transform/2]).


parse_transform(Forms, _Options) ->
    case get_required_records(Forms) of
	[] -> Forms;
	ReqRecs -> 
	    true = has_behaviour(Forms),
	    nakaz_record_parser:insert_specs_getter(Forms, ReqRecs)
    end.

get_required_records(Forms) ->
    find_all_calls(Forms).

find_all_calls(Forms) ->
    R = parse_trans:inspect(fun inspect_call/4,
			    [],
			    Forms,
			    []),
    io:format("Applications: ~p ~n", [R]),
    [config].

inspect_call(application, Form, _Context, Acc) ->
    {false, [Form | Acc]};
inspect_call(_, _, _, Acc) ->
    {true, Acc}.

has_behaviour(Forms) ->
     lists:any(fun is_behaviour/1, Forms).

is_behaviour({attribute,_,behavior,nakaz_user}) ->
    true;
is_behaviour({attribute,_,behaviour,nakaz_user}) ->
    true;
is_behaviour(_) ->
    false.


