%%%-------------------------------------------------------------------
%%% @author Gustavo Feuerstein
%%% @copyright (C) 2019, Robot Genesis
%%% @doc
%%%
%%% @end
%%% Created : 16. mai 2019 14:54
%%%-------------------------------------------------------------------
-module(revolver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([init/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
  start_link().

stop(_State) ->
  ok.

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  
  RevolverMonitor = #{
    id => revolver_monitor,
    start => {revolver_monitor, start_link, []},
    restart => permanent,
    type => supervisor,
    modules => [revolver_monitor]
  },
  
  Gun = #{
    id => gun_application,
    start => {gun_sup, start_link, []},
    restart => permanent,
    type => supervisor,
    modules => [gun_sup]
  },
  
  {ok, {SupFlags, [Gun, RevolverMonitor]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).