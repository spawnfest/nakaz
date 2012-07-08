%%% @author Sergei Levedev <superbobry@gmail.com>
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

%% Types

-type composer_events() :: [yaml_libyaml:event()].

-record(state, {anchors :: dict(),
                events  :: composer_events()}).


-type composer_node()     :: term(). %% Actual type: raw_field().
-type composer_mapping()  :: [{atom(), composer_node()}].
-type composer_sequence() :: [composer_node()].
-type composer_anchor()   :: null | binary().
-type composer_document() :: [composer_node()].

%% API

-spec compose([yaml_libyaml:event()])
             -> {ok, raw_config()}
              | {error, composer_error(), raw_position()}.
compose([{stream_start, _, _, _}|Events]) ->
    try compose_documents(#state{anchors=dict:new(), events=Events}) of
        Docs when is_list(Docs) -> {ok, Docs}
    catch
        _:{error, {Reason, <<_>>}, {Line, Col}}=Error
          when is_integer(Line), is_integer(Col),
               (Reason == unknown_anchor orelse
                Reason == duplicate_anchor) -> Error;
        _:{error, {duplicate_key, Key}, {Line, Col}}=Error
          when is_atom(Key), is_integer(Line), is_integer(Col) -> Error
    end.

%% Internal

-spec compose_documents(#state{}) -> [composer_document()].
compose_documents(State) ->
    compose_documents(State, []).

-spec compose_documents(#state{}, [composer_document()])
                       -> [composer_document()].
compose_documents(State, Acc) ->
    case compose_document(State) of
        {finished, #state{}} -> lists:reverse(Acc);
        {continue, {undefined, NewState}} -> compose_documents(NewState, Acc);
        {continue, {Doc, NewState}} -> compose_documents(NewState, [Doc|Acc])
    end.

-spec compose_document(#state{})
                      -> {continue, {term(), #state{}}}
                       | {continue, {composer_document(), #state{}}}
                       | {finished, #state{}}.
compose_document(#state{events=[{document_start, _, _, _},
                                {document_end, _, _, _}|Events]}=State) ->
    {continue, {undefined, State#state{events=Events}}};
compose_document(#state{events=[{document_start, _, _, _}|Events]}=State) ->
    {Node, NewState} = compose_node(State#state{events=Events}),
    [{document_end, _, _, _}|NewEvents] = NewState#state.events,
    {continue, {Node, State#state{events=NewEvents}}};
compose_document(#state{events=[{stream_end, _, _, _}|Events]}=State) ->
    {finished, State#state{events=Events}}.

-spec compose_node(#state{}) -> {composer_node(), #state{}}.
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

-spec compose_sequence(#state{}) -> {composer_sequence(), #state{}}.
compose_sequence(State) ->
    compose_sequence(State, []).

-spec compose_sequence(#state{}, composer_sequence())
                      -> {composer_sequence(), #state{}}.
compose_sequence(#state{events=[{sequence_end, _, _, _}|Events]}=State, Acc) ->
    {lists:reverse(Acc), State#state{events=Events}};
compose_sequence(State, Acc) ->
    {Value, NewState} = compose_node(State),
    compose_sequence(NewState, [Value|Acc]).

-spec compose_mapping(#state{}) -> {composer_mapping(), #state{}}.
compose_mapping(State) ->
    compose_mapping(State, dict:new()).

-spec compose_mapping(#state{}, dict()) -> {composer_mapping(), #state{}}.
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

-spec maybe_anchor(composer_anchor(), composer_node(), #state{})
                  -> {ok, #state{}}
                   | {error, {duplicate_anchor, composer_anchor()}}.
maybe_anchor(null, _Node, State) ->
    {ok, State};
maybe_anchor(Anchor, Node, #state{anchors=Anchors}=State) ->
	case dict:is_key(Anchor, Anchors) of
		true  -> {error, {duplicate_anchor, Anchor}};
        false -> {ok, State#state{anchors=dict:store(Anchor, Node, Anchors)}}
    end.

-spec compose_error(composer_error(),
                    {_, pos_integer(), pos_integer()}) -> no_return().
compose_error(Reason, {_, Line, Column}) ->
    throw({error, Reason, {Line, Column}}).
