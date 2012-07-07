-module(nakaz_schema).

-export([init/1, destroy/1,
         resolve_mapping_tag/3, resolve_sequence_tag/3, resolve_scalar_tag/4,
         construct_mapping/3, construct_sequence/3, construct_scalar/3]).

-record(state, {tag_regexs :: [{binary(), atom()}]}).

init([]) ->
    %% FIXME(Sergei): allow atoms?
    %% {<<"^[a-z][a-zA-Z0-9_@]*$">>, atom}
    Regexs =
        [{<<"^[-+]?[0-9]+$">>, integer},
         {<<"^0o[0-7]+$">>, integer},
         {<<"^0x[0-9a-fA-F]+$">>, integer},
         {<<"^[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?$">>, float}],
    CompiledRegexs = [begin
                          {ok, Compiled} = re:compile(Regex),
                          {Compiled, Tag}
                      end || {Regex, Tag} <- Regexs],
    #state{tag_regexs=CompiledRegexs}.

destroy(_State) ->
    ok.

resolve_mapping_tag(Tag, Value, State) ->
    yaml_schema_failsafe:resolve_mapping_tag(Tag, Value, State).

construct_mapping(Tag, Value, State) ->
    yaml_schema_failsafe:construct_mapping(Tag, Value, State).

resolve_sequence_tag(Tag, Value, State) ->
    yaml_schema_failsafe:resolve_sequence_tag(Tag, Value, State).

construct_sequence(Tag, Value, State) ->
    yaml_schema_failsafe:construct_sequence(Tag, Value, State).

resolve_scalar_tag(null, <<"undefined">>, plain, _State) -> {ok, undefined};
resolve_scalar_tag(null, <<"~">>, plain, _State)     -> {ok, undefined};
resolve_scalar_tag(null, <<"true">>, plain, _State)  -> {ok, boolean};
resolve_scalar_tag(null, <<"false">>, plain, _State) -> {ok, boolean};
resolve_scalar_tag(null, Value, plain, #state{tag_regexs=Regexs}) ->
    Guesses =
        lists:dropwhile(
          fun ({Rexp, _Tag}) ->
                  re:run(Value, Rexp, [{capture, none}]) =:= nomatch
          end,
          Regexs),

    case Guesses of
        [] -> {ok, binary};
        [{_, Tag}|_] -> {ok, Tag}
    end;
resolve_scalar_tag(null, _Value, _Style, _State) -> {ok, binary};
resolve_scalar_tag(_Tag, _Value, _Style, _State)  -> nomatch.

construct_scalar(Tag, Value, _State) ->
    construct_scalar(Tag, Value).

construct_scalar(undefined, _) -> {ok, undefined};
construct_scalar(boolean, <<"true">>)  -> {ok, true};
construct_scalar(boolean, <<"false">>) -> {ok, false};
construct_scalar(integer, Value) ->
    {Base, IntPart} = case Value of
        <<"0x",P/binary>> -> {16, P};
        <<"0o",P/binary>> -> {8, P};
        P                 -> {10, P}
    end,
    case catch list_to_integer(binary_to_list(IntPart), Base) of
        Int when is_integer(Int) -> {ok, Int};
        _ -> badarg
    end;
construct_scalar(float, Value) ->
    case string:to_float(binary_to_list(Value)) of
        {Num, []} -> {ok, Num};
        _ ->
            %% FIXME(Sergei): does Erlang support +-Inf and +-NaN?
            badarg
    end;
construct_scalar(binary, Value) -> {ok, Value};
%% FIXME(Sergei): allow atoms?
%% construct_scalar(atom, Value) ->
%%     case catch binary_to_atom(Value, utf8) of
%%         Atom when is_atom(Atom) -> {ok, Atom};
%%         _Error -> nomatch
%%     end;
construct_scalar(_Tag, _Value) -> nomatch.
