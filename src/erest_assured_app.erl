%%%-------------------------------------------------------------------
%%% @author Karam Kim
%%% @copyright (C) 2017, Twinny
%%% @doc
%%%
%%% @end
%%% Created : 06. 3월 2017 오후 5:54
%%%-------------------------------------------------------------------
-module(erest_assured_app).
-author("Karam Kim").

%% API
-export([start/0]).

-define(APP, erest_assured_app).

start() ->
  application:load(?APP),
  {ok, Apps} = application:get_key(?APP, applications),
  [application:start(App) || App <- Apps],
  application:start(?APP).
