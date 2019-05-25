%%%-------------------------------------------------------------------
%%% @author Gustavo Feuerstein
%%% @copyright (C) 2019, Robot Genesis
%%% @doc
%%%
%%% @end
%%% Created : 16. mai 2019 14:57
%%%-------------------------------------------------------------------
-module(revolver_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([headers/2]).
-export([whois/1]).
-export([connect/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {connections = #{}}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{debug, [debug]}]).

headers(Headers, ConnectionName) ->
  gen_server:call(?SERVER, {headers, Headers, ConnectionName}, infinity).

connect(ConnectionName) ->
  gen_server:call(?SERVER, {connect, ConnectionName}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  try
    io:format("Connecting to: ~w~n", [env:get_atoms(connections)]),
    Connections = env:get_maps(connections),
    {ok, #state{connections = Connections}}
  catch
    error:Reason -> {stop, Reason}
  end.

%%--------------------------------------------------------------------

handle_call({headers, Headers, ConnectionName}, _From, State) ->
  #{ConnectionName := Properties} = State#state.connections,
  %HostHeader = {<<"Host">>, list_to_binary(maps:get(host, Properties))},
  KeepAlive = {<<"Connection">>, <<"keep-alive">>},
  AuthorizationHeader = case maps:find(username, Properties) of
                          {ok, Username} ->
                            Password = maps:get(password, Properties),
                            authorization_header(Username, Password);
                          error -> []
                        end,
  CacheControl = {<<"Cache-Control">>, <<"no-cache">>},
  
  {reply, lists:flatten(Headers ++ [KeepAlive, AuthorizationHeader, CacheControl]), State};

handle_call({connect, ConnectionName}, _From, State) ->
  ConnPid = case whereis(ConnectionName) of
              undefined ->
                #{ConnectionName := Properties} = State#state.connections,
                connect_and_register(ConnectionName, Properties);
              Pid -> Pid
            end,
  {reply, ConnPid, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------

handle_info({'DOWN', _Ref, process, _ConnPid, _Reason}, State) ->
  {noreply, State};

handle_info({gun_down, ConnPid, _Protocol, closed, [], []}, State) ->
  Name = whois(ConnPid),
  io:format("Disconnected: ~s~n", [Name]),
  gun:close(ConnPid),
  {noreply, State};

handle_info({gun_up, ConnPid, _Protocol}, State) ->
  Name = whois(ConnPid),
  io:format("Reconnected: ~s~n", [Name]),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect_and_register(ConnectionName, Properties) ->
  #{host := Host} =  Properties,
  Port = maps:get(port, Properties, 443),
  ConnPid = connect(Host, Port),
  io:format("Connected to: ~s~n", [ConnectionName]),
  register_(ConnPid, ConnectionName),
  ConnPid.

connect(Host, Port) ->
  Options = #{protocols => [http], trace => false},
  ConnPid = case gun:open(Host, Port, Options) of
              {ok, LConnPid} -> LConnPid;
              {error, OReason} -> error(OReason)
            end,
  case gun:await_up(ConnPid) of
    {ok, _Protocol}  -> ConnPid;
    {error, timeout} -> error(not_connected);
    {error, WReason} -> error(WReason)
  end.

register_(ConnPid, Name) ->
  erlang:register(Name, ConnPid),
  monitor(process, Name).

%%--------------------------------------------------------------------

authorization_header(Username, Password) ->
  {<<"Authorization">>, list_to_binary([<<"Basic ">>, base64:encode(iolist_to_binary([Username, ":", Password]))])}.

whois(Pid) ->
  try
    {_, Assistant} = erlang:process_info(Pid, registered_name),
    Assistant
  catch
    error:_ -> unknown
  end.
  
