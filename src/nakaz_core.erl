-module(nakaz_core).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([use/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%% API
start_link(ConfPath) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ConfPath], []).

use(Mod, App, Key) ->
    gen_server:call(?SERVER, {use, Mod, App, Key}).

%%% gen_server callbacks
init([ConfPath]) ->
    io:format("ConfPath: ~p~n", [ConfPath]),
    {ok, #state{}}.

handle_call({use, Mod, App, Key}, _From, State) ->
    {reply, {Mod, App, Key}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
