%%%-------------------------------------------------------------------
%%% @author Gustavo Feuerstein
%%% @copyright (C) 2018, Robot Genesis
%%% @doc
%%%
%%% @end
%%% Created : 19. dez 2018 12:16
%%%-------------------------------------------------------------------
-module(revolver).

-export([get/2, get/3]).
-export([post/2, post/3, post/4]).
-export([put/3, put/4]).
-export([delete/2, delete/3]).
-export([build_query/2, build_query/3]).
-export([test/0]).

%%%===================================================================
%%% API
%%%===================================================================

get(Connection, Query) ->
  get(Connection, Query, []).
  
get(Connection, Query, Headers0) ->
  try
    Headers1 = revolver_monitor:headers(Headers0, Connection),
    ConnPid = revolver_monitor:connect(Connection),
    StreamRef = gun:get(ConnPid, Query, Headers1),
    handle_response(ConnPid, StreamRef)
  catch
    error:Reason -> {error, Reason}
  end.

%%%===================================================================

post(Connection, Query) ->
  post(Connection, Query, <<>>).

post(Connection, Query, Content) ->
  post(Connection, Query, Content, [{<<"Content-Type">>, <<"application/json">>}]).

post(Connection, Query, Content, Headers0) ->
  try
    Headers1 = revolver_monitor:headers(Headers0, Connection),
    ConnPid = revolver_monitor:connect(Connection),
    StreamRef = gun:post(ConnPid, Query, Headers1, Content),
    handle_response(ConnPid, StreamRef)
  catch
    error:Reason -> {error, Reason}
  end.

%%%===================================================================

put(Connection, Query, Content) ->
  put(Connection, Query, Content, [{<<"Content-Type">>, <<"application/json">>}]).

put(Connection, Query, Content, Headers0) ->
  try
    Headers1 = revolver_monitor:headers(Headers0, Connection),
    ConnPid = revolver_monitor:connect(Connection),
    StreamRef = gun:put(ConnPid, Query, Headers1, Content),
    handle_response(ConnPid, StreamRef)
  catch
    error:Reason -> {error, Reason}
  end.

%%%===================================================================

delete(Connection, Query) ->
  delete(Connection, Query, []).

delete(Connection, Query, Headers0) ->
  try
    Headers1 = revolver_monitor:headers(Headers0, Connection),
    ConnPid = revolver_monitor:connect(Connection),
    StreamRef = gun:delete(ConnPid, Query, Headers1),
    handle_response(ConnPid, StreamRef)
  catch
    error:Reason -> {error, Reason}
  end.

%%%===================================================================

build_query(Action0, ActionParameters, Parameters) ->
  Action2 = io_lib:format(Action0, ActionParameters),
  build_query(Action2, Parameters).

build_query(Action, []) ->
  lists:flatten(Action);

build_query(Action, Parameters) ->
  Properties = [query_format(Key, Value) || {Key, Value} <- Parameters],
  Query = io_lib:format("~s?~s", [Action, query_join(Properties)]),
  lists:flatten(Query).

%%%===================================================================
%%% Internal functions
%%%===================================================================

query_format(Key, Value) ->
  io_lib:format("~s=~s", [Key, edoc_lib:escape_uri(Value)]).

query_join(Properties) ->
  [_|T] = lists:foldl(
    fun(P, Acc) ->
      lists:concat([Acc, "&", P])
    end, [], Properties),
  T.

%%%===================================================================

handle_response(ConnPid, StreamRef) ->
  GetResponse =  gun:await(ConnPid, StreamRef),
  case GetResponse of
    {response, _Fin, Status, _Headers} ->
      case gun:await_body(ConnPid, StreamRef) of
        {ok, Response} ->
          case Status of
            Status_ when Status_ < 300 -> {ok, Response};
            Other -> error({http_status, {Other, Response}})
          end;
        {error, BReason} -> error({http_response, BReason})
      end;
    {error, HReason} -> error({http, HReason})
  end.

%%%===================================================================

test() ->
  observer:start(),
  _A = application:ensure_all_started(revolver),
  application:stop(revolver).
