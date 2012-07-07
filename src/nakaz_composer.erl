-module(nakaz_composer).

-export([compose/1, compose/2]).

-record(state, {anchors :: dict(),
                schema  :: {Module :: atom(), State :: any()},
                events  :: [yaml_libyaml:event()]}).

%% API

-spec compose([yaml_libyaml:event()])
             -> {ok, [{atom(), term()}]} | {error, any()}.
compose(Events) ->
    compose(Events, []).

-spec compose([yaml_libyaml:event()], [any()])
             -> {ok, [{atom(), term()}]} | {error, any()}.
compose([{stream_start, _, _, _}|Events], Opts) ->
    Schema = proplists:get_value(schema, Opts, yaml_schema_erlang),
    try
        compose_documents(#state{anchors=dict:new(),
                                 schema={Schema, Schema:init(Opts)},
                                 events=Events})
    catch
        _:{error, _Reason}=Error -> Error
    end.

%% Internal

compose_documents(State) ->
    compose_documents(State, []).

compose_documents(State, Acc) ->
    case compose_document(State) of
        {continue, {undefined, NewState}} -> compose_documents(NewState, Acc);
        {continue, {Doc, NewState}} -> compose_documents(NewState, [Doc|Acc]);
        {finished, _NewState} -> lists:reverse(Acc)
    end.

compose_document(#state{events=[{document_start, _, _, _},
                                {document_end, _, _, _}|Events]}=State) ->
    {continue, {undefined, State#state{events=Events}}};
compose_document(#state{events=[{document_start, _, _, _}|Events]}=State) ->
    {{_ResolvedTag, Node}, NewState} = compose_node(State#state{events=Events}),
    [{document_end, _, _, _}|NewEvents] = NewState#state.events,
    {continue, {Node, State#state{events=NewEvents}}};
compose_document(#state{events=[{stream_end, _, _, _}|Events]}=State) ->
    {finished, State#state{events=Events}}.

compose_node(#state{events=[{scalar, Body, Start, _}|Events],
                    schema={Schema, SchemaState}}=State) ->
    {Anchor, Tag, Value, Style} = Body,
    ResolvedTag =
        case Schema:resolve_scalar_tag(Tag, Value, Style, SchemaState) of
            {ok, ActualTag} -> ActualTag;
            _               -> compose_error({unknown_tag, Tag}, Start)
        end,
    ConstructedValue =
        case Schema:construct_scalar(ResolvedTag, Value, SchemaState) of
            {ok, ActualValue} -> ActualValue;
            _                 -> compose_error({invalid_scalar, Value},
                                               Start)
        end,

    Node = {ResolvedTag, ConstructedValue},
    case maybe_anchor(Anchor, Node, State#state{events=Events}) of
        {ok, NewState}  -> {Node, NewState};
        {error, Reason} -> compose_error(Reason, Start)
    end;
compose_node(#state{events=[{alias, Anchor, Start, _}|Events],
                    anchors=Anchors}=State) ->
    case dict:find(Anchor, Anchors) of
        {ok, Node} ->
            {Node, State#state{events=Events}};
        error ->
            compose_error({unknown_anchor, Anchor}, Start)
    end;
compose_node(#state{events=[{sequence_start, Body, Start, _}|Events],
                    schema={Schema, SchemaState}}=State) ->
    {Anchor, Tag, _Style} = Body,
    {Nodes, NewState} = compose_sequence(State#state{events=Events}),
    ResolvedTag =
        case Schema:resolve_sequence_tag(Tag, Nodes, SchemaState) of
            {ok, ActualTag} -> ActualTag;
            _               -> compose_error({unknown_tag, Tag}, Start)
        end,
    ConstructedValue =
        case Schema:construct_sequence(ResolvedTag, Nodes, SchemaState) of
            {ok, ActualValue} -> ActualValue;
            _                 -> compose_error({invalid_sequence, Nodes},
                                               Start)
        end,

    Node = {ResolvedTag, ConstructedValue},
    case maybe_anchor(Anchor, Node, NewState) of
        {ok, NewestState} -> {Node, NewestState};
        {error, Reason}   -> compose_error(Reason, Start)
    end;
compose_node(#state{events=[{mapping_start, Body, Start, _}|Events],
                    schema={Schema, SchemaState}}=State) ->
    {Anchor, Tag, _Style} = Body,
    {Nodes, NewState} = compose_mapping(State#state{events=Events}),
    ResolvedTag =
        case Schema:resolve_mapping_tag(Tag, Nodes, SchemaState) of
            {ok, ActualTag} -> ActualTag;
            _               -> compose_error({unknown_tag, Tag}, Start)
        end,
    ConstructedValue =
        case Schema:construct_mapping(ResolvedTag, Nodes, SchemaState) of
            {ok, ActualValue} -> ActualValue;
            _                 -> compose_error({invalid_mapping, Nodes},
                                               Start)
        end,

    Node = {ResolvedTag, ConstructedValue},
    case maybe_anchor(Anchor, Node, NewState) of
        {ok, NewState}  -> {Node, NewState};
        {error, Reason} -> compose_error(Reason, Start)
    end.

compose_sequence(State) ->
    compose_sequence(State, []).

compose_sequence(#state{events=[{sequence_end, _, _, _}|Events]}=State, Acc) ->
    {lists:reverse(Acc), State#state{events=Events}};
compose_sequence(State, Acc) ->
    {{_ResolvedTag, ConstructedValue}, NewState} = compose_node(State),
    compose_sequence(NewState, [ConstructedValue|Acc]).

compose_mapping(State) ->
    compose_mapping(State, dict:new()).

compose_mapping(#state{events=[{mapping_end, _, _, _}|Events]}=State, Acc) ->
	{dict:to_list(Acc), State#state{events=Events}};
compose_mapping(#state{events=[{_, _, Start, _}|_Events]}=State, Acc) ->
    {{_KeyTag, Key}, State1} = compose_node(State),
    {{_ValueTag, Value}, State2} = compose_node(State1),
    case dict:is_key(Key, Acc) of
        false -> compose_mapping(State2, dict:store(Key, Value, Acc));
        true  -> compose_error({duplicate_key, Key}, Start)
    end.

maybe_anchor(null, _Node, State) ->
    {ok, State};
maybe_anchor(Anchor, Node, #state{anchors=Anchors}=State) ->
	case dict:is_key(Anchor, Anchors) of
		true  -> {error, {duplicate_anchor, Anchor}};
        false -> {ok, State#state{anchors=dict:store(Anchor, Node, Anchors)}}
    end.

compose_error(Reason, {_, Line, Column}) ->
    throw({error, Reason, {Line, Column}}).
