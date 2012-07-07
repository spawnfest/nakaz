-module(nakaz_core).
-behaviour(gen_server).
-include("nakaz_internal.hrl").
-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/1]).
-export([ensure/4, use/3, reload/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {config_path :: string(),
                reload_type :: reload_type()}).

%%% API
%% FIXME(Dmitry): add typespecs

start_link(ConfPath) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ConfPath], []).

ensure(Mod, App, Records, Options) ->
    gen_server:call(?SERVER, {ensure, Mod, App, Records, Options}).

use(Mod, App, Record) ->
    gen_server:call(?SERVER, {use, Mod, App, Record}).

reload() ->
    gen_server:call(?SERVER, reload).

%%% gen_server callbacks
init([ConfPath]) ->
    ets:new(nakaz_apps, [named_table]),
    ets:new(nakaz_registry, [named_table, bag]),
    {ok, #state{config_path=ConfPath}}.

handle_call({ensure, Mod, _App, _Records, Options}, _From, State) ->
    ReloadType = proplists:get_value(reload_type, Options, async),
    case read_config(State#state.config_path) of
        {error, _Reason}=E ->
            {reply, E, State};
        {ok, _RawConfig} ->
            RecordSpecs = Mod:?NAKAZ_ENSURE_MAGIC(),
            {reply, RecordSpecs, State#state{reload_type=ReloadType}}
    end;
handle_call({use, Mod, App, Record}, _From, State) ->
    RecordName = erlang:element(1, Record),
    ets:insert(nakaz_registry, {{App, RecordName}, Mod}),
    {reply, ok, State};
handle_call(Request, _From, State) ->
    lager:warning("Unhandled call ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("Unhandled info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
read_config(Path) ->
    file:consult(Path).
