%%%-------------------------------------------------------------------
%%% @author zntuszntus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 3월 2017 오전 10:12
%%%-------------------------------------------------------------------
-module(erest_assured).
-author("zntuszntus").

-define(APP, erest_assured).

-type giver() :: function().
-type givers() :: list(giver()).
-type requester() :: function().
-type assert() :: function().
-type asserts() :: list(assert()).
-type tester() :: function().
-type assured() :: ok | term().

-export([start/0, start/1]).

-export([
  assured/4,
  given/1,
  request/1,
  then/1
]).

-export([
  param/2
]).

-export([
  get/1, post/1, put/1, option/1, delete/1, patch/1
]).

-export([
  body/1
]).

-export([
  equal_to/1
]).

start() ->
  application:load(?APP),
  {ok, Apps} = application:get_key(?APP, applications),
  [application:start(App) || App <- Apps],
  application:start(?APP).

start(App) ->
  application:start(App, permanent).

-spec assured(bitstring(), function(), function(), function()) -> assured().
assured(Describe, Given, Requester, Then) ->
  Init = erest_request:new(),
  Request = Given(Init),
  Response = Requester(Request),
  Assured = Then(Describe, Response),
  Assured.

-spec given(givers()) -> function().
given(Givers) when is_list(Givers) ->
  fun(Request) ->
    lists:foldl(
      fun(Giver, R) -> Giver(R) end,
      Request,
      Givers
    )
  end.

-spec request(requester()) -> function().
request(Requester) -> Requester.

-spec then(asserts()) -> function().
then(Asserts) when is_list(Asserts) ->
  fun(Describe, Response) ->
    lists:foldl(
      fun
        (Assert, {ok, _}) ->
          R = Assert(Describe, Response),
          print_assert(R);
        (_Assert, Fail) ->
          Fail
      end,
      {ok, {}},
      Asserts
    )
  end.

-spec param(atom(), bitstring()) -> giver().
param(Param, Value) ->
  fun(Request) ->
    erest_request:add_parameter({Param, Value}, Request)
  end.

-spec get(string()) -> requester().
get(Path) -> gen_execute_request(get, Path).

-spec post(string()) -> requester().
post(Path) -> gen_execute_request(post, Path).

-spec put(string()) -> requester().
put(Path) -> gen_execute_request(put, Path).

-spec option(string()) -> requester().
option(Path) -> gen_execute_request(option, Path).

-spec delete(string()) -> requester().
delete(Path) -> gen_execute_request(delete, Path).

-spec patch(string()) -> requester().
patch(Path) -> gen_execute_request(patch, Path).

-spec body(tester()) -> assert().
body(Tester) ->
  fun(Describe, Response) ->
    Tester(Describe, erest_response:body(Response))
  end.

-spec equal_to(term()) -> tester().
equal_to(Expected) ->
  fun(Describe, Value) ->
    assert_it(Describe, "should be equal", Expected, Value, Expected =:= Value)
  end.

%%
%% internal functions
%%

gen_execute_request(Method, Path) ->
  fun(Request0) ->
    Request1 = erest_request:path(Path, Request0),
    Request  = erest_request:method(Method, Request1),
    erest_request:execute(Request)
  end.

assert_it(Describe, Msg, Expected, Value, true) -> {ok, {Describe, Msg, Expected, Value}};
assert_it(Describe, Msg, Expected, Value, false) -> {fail, {Describe, Msg, Expected, Value}}.

print_assert({ok, {Describe, _, _, _}} = Assert) ->
  io:format("~s success", [Describe]),
  Assert;
print_assert({fail, {Describe,  Msg, Expected, Value}} = Assert) ->
  io:format("~s ~s but fail !!", [Describe, Msg]),
  io:format("Expected: ~p\n\rActual: ~p", [Expected, Value]),
  Assert.