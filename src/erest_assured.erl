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
  param/2, host/1, port/1
]).

-export([
  get/1, post/1, put/1, option/1, delete/1, patch/1
]).

-export([
  body/1, json/1
]).

-export([
  equal_to/1, equal_to/2, is_json/0,
  should_be_string/1, should_be_integer/1, should_be_float/1, should_be_number/1,
  has_key/1
]).

start() ->
  application:load(?APP),
  {ok, Apps} = application:get_key(?APP, applications),
  [application:ensure_all_started(App) || App <- Apps],
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
    Result =
      lists:foldl(
        fun
          (Assert, {ok, _}) -> Assert(Describe, Response);
          (_Assert, Fail)   -> Fail
        end,
        {ok, {}},
        Asserts
      ),
    print_assert(Result),
    case Result of
      {ok, _} -> ok;
      _ -> Result
    end
  end.

-spec param(atom(), bitstring()) -> giver().
param(Param, Value) ->
  fun(Request) ->
    erest_request:add_parameter({Param, Value}, Request)
  end.

-spec host(string()) -> giver().
host(Host) ->
  fun(Request) ->
    erest_request:host(Host, Request)
  end.

-spec port(pos_integer()) -> giver().
port(Port) ->
  fun(Request) ->
    erest_request:port(Port, Request)
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
    assert_it(Describe, "should be equal with expected value", Expected, Value, Expected =:= Value)
  end.

-spec is_json() -> tester().
is_json() ->
  fun(Describe, Value) ->
    Result =
      try jsx:decode(Value) of
        _ -> true
      catch
        error:badarg -> false
      end,
    assert_it(Describe, "should be JSON", Value, Result)
  end.

-spec json(tester()) -> assert().
json(Tester) ->
  fun(Describe, Response) ->
    Tester(Describe, jsx:decode(erest_response:body(Response)))
  end.

-spec equal_to(string(), term()) -> tester().
equal_to(Key, Expected) ->
  fun(Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, Key ++ " should be equal with expected value", Expected, Value, Expected =:= Value)
  end.

-spec should_be_string(string()) -> tester().
should_be_string(Key) ->
  fun(Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, Key ++ " should be string", Value, is_bitstring(Value))
  end.

-spec should_be_integer(string()) -> tester().
should_be_integer(Key) ->
  fun(Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, Key ++ " should be integer", Value, is_integer(Value))
  end.

-spec should_be_float(string()) -> tester().
should_be_float(Key) ->
  fun(Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, Key ++ " should be float", Value, is_float(Value))
  end.

-spec should_be_number(string()) -> tester().
should_be_number(Key) ->
  fun(Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, Key ++ " should be number", Value, is_integer(Value) orelse is_float(Value))
  end.

-spec has_key(string()) -> tester().
has_key(Key) ->
  fun(Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, Key ++ " should be exist", Value =/= undefined)
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

assert_it(Describe, Msg, true) -> {ok, {Describe, Msg}};
assert_it(Describe, Msg, false) -> {fail, {Describe, Msg}}.
assert_it(Describe, Msg, Value, true) -> {ok, {Describe, Msg, Value}};
assert_it(Describe, Msg, Value, false) -> {fail, {Describe, Msg, Value}}.
assert_it(Describe, Msg, Expected, Value, true) -> {ok, {Describe, Msg, Expected, Value}};
assert_it(Describe, Msg, Expected, Value, false) -> {fail, {Describe, Msg, Expected, Value}}.

print_assert({ok, {Describe, _}} = Assert) ->
  io:format("~s success~n", [Describe]),
  Assert;
print_assert({ok, {Describe, _Msg, _}} = Assert) ->
  print_assert({ok, {Describe, _Msg}}),
  Assert;
print_assert({ok, {Describe, _Msg, _Value, _}} = Assert) ->
  print_assert({ok, {Describe, _Msg, _Value}}),
  Assert;
print_assert({fail, {Describe,  Msg}} = Assert) ->
  io:format("~s ~s !!~n", [Describe, Msg]),
  Assert;
print_assert({fail, {Describe,  Msg, Value}} = Assert) ->
  print_assert({fail, {Describe,  Msg}}),
  io:format("Value: ~p~n", [Value]),
  Assert;
print_assert({fail, {Describe,  Msg, Expected, Value}} = Assert) ->
  print_assert({fail, {Describe,  Msg}}),
  io:format("Expected: ~p~nValue: ~p~n", [Expected, Value]),
  Assert.

get_value_by_json_path(JSON, Path) ->
  Paths = string:tokens(Path, "."),
  get_value_by_json_path(loop, JSON, Paths).
get_value_by_json_path(loop, undefine, _) -> undefined;
get_value_by_json_path(loop, Value, []) -> Value;
get_value_by_json_path(loop, JSON, [Key | T]) ->
  NextJSON = proplists:get_value(list_to_binary(Key), JSON, undefined),
  get_value_by_json_path(loop, NextJSON, T).
