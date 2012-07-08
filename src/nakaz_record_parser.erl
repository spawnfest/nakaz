-module(nakaz_record_parser).

-export([extract_records_specs/2]).

-define(BUILTIN_TYPES, [any,binary,integer,pos_integer,neg_integer,non_neg_integer,
			range,number,string,nonempty_string,module,node,timeout,
			none,byte,char,nil,list,nonempty_list,tuple,float,
			record,boolean,atom,union]).

%% FIXME(Dmitry): spec
extract_records_specs(Forms,Module) ->
    lists:foldl(fun (F,Acc) -> handle_type(F,Acc,Module) end,
                [],
                Forms).

%% FIXME(Dmitry): spec
handle_type(Form, Acc, Module) ->
    case erl_syntax_lib:analyze_form(Form) of
        {attribute, {type, {type, Type}}} ->
            handle_record(Type, Acc, Module);
        _ ->
            Acc
    end.

%% Handle only records types
%% FIXME(Dmitry): spec
handle_record({{record, Name}, Fields, _Args},
              Acc,
              Module) ->
    Flds = try 
	       [handle_field(Field, Module) || Field <- Fields] 
	   catch
	       throw:{unsupported_field,
               Form,
		      Module} ->  {unsupported,
				   element(2,Form), %FIXME: Form always has 3 elements?
				   Module}
    end,
    %% Update ordset only if record is one of already referenced
    [{Name, Flds} | Acc];
handle_record(_, Acc, _) ->
    Acc.

%% FIXME(Dmitry): spec
handle_field({typed_record_field,
              {record_field,_,{atom,_,Name}}, Type}, Module) ->
    Field = handle_field_type(Type, Module),
    {Name, Field, undefined};
%% Handle special case with default value
handle_field({typed_record_field,
              {record_field,_,{atom,_,Name}, Default}, Type}, Module) ->
    Field = handle_value_param(Type, Module),
    Def = handle_value_param(Default, Module),
    {Name, Field, Def};
handle_field(Other, Module) ->
    throw({unsupported_field, Other, Module}).

%%FIXME: Only allow typed fields
%%FIXME: Maybe there are different orders of 'undefined' atom
%%       and other term in union
%% FIXME(Dmitry): spec
handle_field_type({type,_,union,
		   [{atom,_,undefined}|
		    Types]}, Module) ->
    handle_union(Types, Module);
handle_field_type(Other, Module) ->
    throw({unsupported_field, Other, Module}).

%% FIXME(Dmitry): spec
handle_union([Type], Module) ->
    handle_value_param(Type, Module);
handle_union(Types, Module) ->
    {get_module_for_type(union, Module),
     union,
     [handle_value_param(Type, Module)
      || Type <- Types]}.

%% FIXME(Dmitry): spec
handle_value_param({remote_type, _, [{atom,_,Module},
                                      {atom,_,Type},
                                      Args]}, _Module) ->
    {Module,
     Type,
     [handle_value_param(Arg, Module) || Arg <- Args]};
handle_value_param({type,_,record,[]}=Form, Module) ->
    %% We do not support generic record type
    throw({unsupported_field, Form, Module});
handle_value_param({type, _, Type, Args}, Module) when is_list(Args) ->
    {get_module_for_type(Type,Module),
     Type,
     [handle_value_param(Arg, Module) || Arg <- Args]};
handle_value_param({type,_, Type, Arg}, Module) ->
    %% Handle special case when type arguments is not list
    {get_module_for_type(Type, Module),
     Type,
     [Arg]};
%% FIXME: rewrite this using erl_syntax_lib for great good.
handle_value_param({atom,_, Atom}, _) ->
    Atom;
handle_value_param({integer,_,Integer}, _) ->
    Integer;
handle_value_param({op,_, '-', {integer,_,Integer}}, _) ->
    %% Unary minus. Special case. Again.
    -Integer;
handle_value_param({float,_,Float}, _) ->
    Float;
handle_value_param({boolean,_,Boolean}, _) ->
    Boolean;
handle_value_param({tuple,_,Values}, Module) ->
    list_to_tuple([handle_value_param(V, Module) || V <- Values]);
handle_value_param({_,LineNo,_}, Module) ->
    throw({unsupported_field, LineNo, Module}).

%% FIXME(Dmitry): spec
get_module_for_type(Type, DefaultModule) ->
    case lists:member(Type, ?BUILTIN_TYPES) of
	true ->
	    undefined;
	false -> DefaultModule
    end.
