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
-type response() :: erest_response:response().

-export([start/0, start/1]).

%% export basic functions
-export([
  assured/4,
  given/1,
  request/1,
  then/1, then/2
]).

%% export givers
-export([
  param/2, host/1, port/1, timeout/1, request_body/1
]).

%% export requesters
-export([
  get/1, post/1, put/1, option/1, delete/1, patch/1
]).

%% export asserts
-export([
  body/1, json/1, status_code/1, time/1
]).

%% export testers
-export([
  equal_to/1, equal_to/2,
  greater_than/1, greater_than/2, greater_than_or_equal_to/1, greater_than_or_equal_to/2,
  less_than/1, less_than/2, less_than_or_equal_to/1, less_than_or_equal_to/2,
  is_json/0, should_be_json/0,
  should_be_string/1, should_be_integer/1, should_be_float/1, should_be_number/1, should_be_list/1,
  has_key/1
]).

%% export getters
-export([
  get_body/1, get_json/1, get_value_from_json/2
]).

-export([
  message_to_list/1, message_to_maps/1
]).

start() ->
  application:load(?APP),
  {ok, Apps} = application:get_key(?APP, applications),
  [application:ensure_all_started(App) || App <- Apps],
  application:start(?APP).

start(App) ->
  application:start(App, permanent).

%%
%% basic functions
%%

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
then(Asserts) -> then(Asserts, []).

-spec then(asserts(), list()) -> function().
then(Asserts, Opts) when is_list(Asserts) ->
  fun(Describe, Response) ->
    Result =
      case erest_response:is_valid(Response) andalso erest_response:is_reachable(Response) of
        true ->
          lists:foldl(
            fun
              (Assert, {ok, _}) -> Assert(Describe, Response);
              (_Assert, Fail)   -> Fail
            end,
            {ok, {}},
            Asserts
          );

        false ->
          {fail, {Describe, "unreachable"}}
      end,

    case list_some(silent, Opts) of
      true -> silent;
      false -> print_assert(Result)
    end,

    {IsOK, Asserted} = Result,
    {IsOK, Asserted, Response}
  end.

%%
%% givers
%%

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

-spec request_body(bitstring()) -> giver().
request_body(Value) ->
  fun(Request) ->
    erest_request:body(Value, Request)
  end.

-spec timeout(pos_integer()) -> giver().
timeout(Time) ->
  fun(Request) ->
    erest_request:timeout(Time, Request)
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

%%
%% asserts
%%

-spec body(tester()) -> assert().
body(Tester) ->
  fun(Describe, Response) ->
    Tester("body", Describe, erest_response:body(Response))
  end.

-spec json(tester()) -> assert().
json(Tester) ->
  fun(Describe, Response) ->
    Tester(json, Describe, jsx:decode(erest_response:body(Response)))
  end.

-spec status_code(tester()) -> assert().
status_code(Tester) ->
  fun(Describe, Response) ->
    Tester("status code", Describe, erest_response:status_code(Response))
  end.

-spec time(tester()) -> assert().
time(Tester) ->
  fun(Describe, Response) ->
    Tester("time duration", Describe, erest_response:time_duration(Response))
  end.

%%
%% testers
%%

-spec greater_than(number()) -> tester().
greater_than(Expected) ->
  fun(What, Describe, Value) ->
    assert_it(Describe, string_combine([What, " should be greater than expected value"]), Expected, Value, Expected < Value)
  end.

-spec greater_than(string(), number()) -> tester().
greater_than(Key, Expected) ->
  fun(json, Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, string_combine([Key, " should be greater than expected value"]), Expected, Value, Expected < Value)
  end.

-spec greater_than_or_equal_to(number()) -> tester().
greater_than_or_equal_to(Expected) ->
  fun(What, Describe, Value) ->
    assert_it(Describe, string_combine([What, " should be greater than or equal to expected value"]), Expected, Value, Expected =< Value)
  end.

-spec greater_than_or_equal_to(string(), number()) -> tester().
greater_than_or_equal_to(Key, Expected) ->
  fun(json, Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, string_combine([Key, " should be greater than or equal to expected value"]), Expected, Value, Expected =< Value)
  end.

-spec less_than(number()) -> tester().
less_than(Expected) ->
  fun(What, Describe, Value) ->
    assert_it(Describe, string_combine([What, " should be less than expected value"]), Expected, Value, Expected > Value)
  end.

-spec less_than(string(), number()) -> tester().
less_than(Key, Expected) ->
  fun(json, Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, string_combine([Key, " should be less than expected value"]), Expected, Value, Expected > Value)
  end.

-spec less_than_or_equal_to(number()) -> tester().
less_than_or_equal_to(Expected) ->
  fun(What, Describe, Value) ->
    assert_it(Describe, string_combine([What, " should be less than or equal to expected value"]), Expected, Value, Expected >= Value)
  end.

-spec less_than_or_equal_to(string(), number()) -> tester().
less_than_or_equal_to(Key, Expected) ->
  fun(json, Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, string_combine([Key, " should be less than or equal to expected value"]), Expected, Value, Expected >= Value)
  end.

-spec equal_to(term()) -> tester().
equal_to(Expected) ->
  fun(What, Describe, Value) ->
    assert_it(Describe, string_combine([What, " should be equal to expected value"]), Expected, Value, Expected =:= Value)
  end.

-spec equal_to(string(), term()) -> tester().
equal_to(Key, Expected) ->
  fun(json, Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, string_combine([Key, " should be equal to expected value"]), Expected, Value, Expected =:= Value)
  end.

-spec is_json() -> tester().
is_json() ->
  fun(What, Describe, Value) ->
    assert_it(Describe, string_combine([What, " should be JSON"]), Value, jsx:is_json(Value))
  end.

-spec should_be_json() -> tester().
should_be_json() -> is_json().

-spec should_be_string(string()) -> tester().
should_be_string(Key) ->
  fun(json, Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, string_combine([Key, " should be string"]), Value, is_bitstring(Value))
  end.

-spec should_be_integer(string()) -> tester().
should_be_integer(Key) ->
  fun(json, Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, string_combine([Key, " should be integer"]), Value, is_integer(Value))
  end.

-spec should_be_float(string()) -> tester().
should_be_float(Key) ->
  fun(json, Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, Key ++ " should be float", Value, is_float(Value))
  end.

-spec should_be_number(string()) -> tester().
should_be_number(Key) ->
  fun(json, Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, string_combine([Key, " should be number"]), Value, is_integer(Value) orelse is_float(Value))
  end.

-spec should_be_list(string()) -> tester().
should_be_list(Key) ->
  fun(json, Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, string_combine([Key, " should be number"]), Value, is_list(Value))
  end.

-spec has_key(string()) -> tester().
has_key(Key) ->
  fun(json, Describe, JSON) ->
    Value = get_value_by_json_path(JSON, Key),
    assert_it(Describe, string_combine([Key, " should be exist"]), Value =/= undefined)
  end.

%%
%% getters
%%

-spec get_body(response()) -> bitstring().
get_body(Response) ->
  erest_response:body(Response).

-spec get_json(response()) -> term().
get_json(Response) ->
  jsx:decode(get_body(Response)).

-spec get_value_from_json(string(), term()) -> term().
get_value_from_json(Key, Response) ->
  JSON = get_json(Response),
  get_value_by_json_path(JSON, Key).


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
  io:format("~s test passed~n", [Describe]),
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
  io:format("  - Value: ~p~n", [Value]),
  Assert;
print_assert({fail, {Describe,  Msg, Expected, Value}} = Assert) ->
  print_assert({fail, {Describe,  Msg}}),
  io:format("  - Expected: ~p~n", [Expected]),
  io:format("  - Value: ~p~n", [Value]),
  Assert.

message_to_list({Describe, Msg}) ->
  [ {describe, Describe},
    {message, Msg} ];
message_to_list({Describe, Msg, Value}) ->
  [ {describe, Describe},
    {message, Msg},
    {value, Value} ];
message_to_list({Describe, Msg, Expected, Value}) ->
  [ {describe, Describe},
    {message, Msg},
    {expected, Expected},
    {value, Value} ].
message_to_maps({Describe, Msg}) ->
  #{ describe => Describe,
     message => Msg };
message_to_maps({Describe, Msg, Value}) ->
  #{ describe => Describe,
     message => Msg,
     value => Value };
message_to_maps({Describe, Msg, Expected, Value}) ->
  #{ describe => Describe,
     message => Msg,
     expected => Expected,
     value => Value }.

get_value_by_json_path(JSON, Path) -> json_path:search(Path, JSON).

string_combine([]) -> [];
string_combine([H|T]) when is_binary(H) ->
  string_combine([binary_to_list(H)|T]);
string_combine([H|T]) ->
  H ++ string_combine(T).

list_some(_Item, []) -> false;
list_some(Fun, [H | T]) when is_function(Fun) ->
  case Fun(H) of
    true -> ture;
    _ -> list_some(Fun, T)
  end;
list_some(Item, [H | _]) when H==Item -> true;
list_some(Item, [_ | T]) -> list_some(Item, T).