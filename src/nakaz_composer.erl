%%% @author Sergey Levedev <superbobry@gmail.com>
%%%
%%% @doc
%%% This module implements a {@see compose/1} function, which, given a
%%% list of `libyaml' events, returns its corresponding Erlang
%%% representation. The current implementation simply drops YAML node
%%% tags, since they will be infered by {@see nakaz_record_parser}
%%% and then converted to the appropriate types by {@see nakaz_typer}.
%%% @end
%%%

-module(nakaz_composer).
-include("nakaz_internal.hrl").

%% API

-export([compose/1]).

-record(state, {anchors :: dict(),
                events  :: [yaml_libyaml:event()]}).

%% API

-spec compose([yaml_libyaml:event()])
             -> {ok, raw_config()}
              | {error, composer_error(), raw_position()}.
compose([{stream_start, _, _, _}|Events]) ->
    try
        Docs = compose_documents(#state{anchors=dict:new(),
                                        events=Events}),
        {ok, Docs}
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
    {Node, NewState} = compose_node(State#state{events=Events}),
    [{document_end, _, _, _}|NewEvents] = NewState#state.events,
    {continue, {Node, State#state{events=NewEvents}}};
compose_document(#state{events=[{stream_end, _, _, _}|Events]}=State) ->
    {finished, State#state{events=Events}}.

compose_node(#state{events=[{scalar, Body, Mark, _}|Events]}=State) ->
    {_Index, Line, Column} = Mark,
    {Anchor, _Tag, Value, _Style} = Body,
    Node = {Value, {Line, Column}},
    case maybe_anchor(Anchor, Node, State#state{events=Events}) of
        {ok, NewState}  -> {Node, NewState};
        {error, Reason} -> compose_error(Reason, Mark)
    end;
compose_node(#state{events=[{alias, Anchor, Mark, _}|Events],
                    anchors=Anchors}=State) ->
    case dict:find(Anchor, Anchors) of
        {ok, Node} ->
            {Node, State#state{events=Events}};
        error ->
            compose_error({unknown_anchor, Anchor}, Mark)
    end;
compose_node(#state{events=[{sequence_start, Body, Mark, _}|Events]}=State) ->
    {_, Line, Column} = Mark,
    {Anchor, _Tag, _Style} = Body,
    {Nodes, NewState} = compose_sequence(State#state{events=Events}),
    Node = {Nodes, {Line, Column}},
    case maybe_anchor(Anchor, Node, NewState) of
        {ok, NewestState} -> {Node, NewestState};
        {error, Reason}   -> compose_error(Reason, Mark)
    end;
compose_node(#state{events=[{mapping_start, Body, Mark, _}|Events]}=State) ->
    {_, Line, Column} = Mark,
    {Anchor, _Tag, _Style} = Body,
    {Nodes, NewState} = compose_mapping(State#state{events=Events}),
    Node = {Nodes, {Line, Column}},
    case maybe_anchor(Anchor, Node, NewState) of
        {ok, NewestState} -> {Node, NewestState};
        {error, Reason}   -> compose_error(Reason, Mark)
    end.

compose_sequence(State) ->
    compose_sequence(State, []).

compose_sequence(#state{events=[{sequence_end, _, _, _}|Events]}=State, Acc) ->
    {lists:reverse(Acc), State#state{events=Events}};
compose_sequence(State, Acc) ->
    {Value, NewState} = compose_node(State),
    compose_sequence(NewState, [Value|Acc]).

compose_mapping(State) ->
    compose_mapping(State, dict:new()).

compose_mapping(#state{events=[{mapping_end, _, _, _}|Events]}=State, Acc) ->
	{dict:to_list(Acc), State#state{events=Events}};
compose_mapping(State, Acc) ->
    {{KeyBin, KeyPos}, State1} = compose_node(State),
    {{_, _}=Value, State2} = compose_node(State1),

    %% Note(Sergei): we drop position information for the key, storing
    %% it in the value instead.
    Key = binary_to_atom(KeyBin, utf8),
    case dict:is_key(Key, Acc) of
        false -> compose_mapping(State2, dict:store(Key, Value, Acc));
        true  -> compose_error({duplicate_key, Key}, KeyPos)
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
