-module(example_srv).
-behaviour(gen_server).
-include_lib("nakaz/include/nakaz.hrl").
-behaviour(nakaz_user).
-include("example.hrl").

%% API
-export([start_link/0]).
-export([configs/0]).

%% nakaz callbacks
-export([nakaz_check/1, nakaz_load/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {srv_conf :: #srv_conf{},
                log_conf :: #log_conf{}}).

%%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

configs() ->
    gen_server:cast(?SERVER, configs).

%% nakaz callbacks
nakaz_check(Conf) ->
    gen_server:call(?SERVER, {nakaz_check, Conf}).

nakaz_load(Conf) ->
    gen_server:call(?SERVER, {nakaz_load, Conf}).


%%% gen_server callbacks
init([]) ->
    SrvConf = ?NAKAZ_USE(#srv_conf{}),
    LogConf = ?NAKAZ_USE(#log_conf{}),
    {ok, #state{srv_conf=SrvConf,
                log_conf=LogConf}}.

handle_call({nakaz_check, _Conf}, _From, State) ->
    {reply, ok, State};
handle_call({nakaz_load, #srv_conf{}=SrvConf}, _From, State) ->
    {reply, ok, State#state{srv_conf=SrvConf}};
handle_call({nakaz_load, #log_conf{}=LogConf}, _From, State) ->
    {reply, ok, State#state{log_conf=LogConf}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(configs, State) ->
    io:format("Example Server uses following configs:~n"
              "   Log Config: ~p~n"
              "   Server Config: ~p~n",
              [State#state.log_conf,
               State#state.srv_conf]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
