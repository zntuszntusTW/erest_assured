%%%-------------------------------------------------------------------
%%% @author Karam Kim
%%% @copyright (C) 2017, Twinny
%%% @doc
%%%
%%% @end
%%% Created : 08. 3월 2017 오후 4:31
%%%-------------------------------------------------------------------
-module(etest).
-author("Karam Kim").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([tests/1, test/1]).
-export([assert_equal/2, assert/1]).

test({Line, Test}) ->
  Describe = proplists:get_value(describe,Test,undefined),
  Run      = proplists:get_value(run,Test,undefined),
  ShouldBe = proplists:get_value(should_be,Test,undefined),
  WithCall = proplists:get_value(with_call,Test,undefined),

  {Describe, {Line,
    fun()->
      with_call(WithCall),
      Result = Run(),
      case ShouldBe of
        undefined -> assert(true);
        _ -> assert_equal(ShouldBe, Result)
      end
    end
  }}.

tests(Tests) when is_list(Tests) -> tests(Tests, []).
tests([], Acc) -> lists:reverse(Acc);
tests([H|T], Acc) -> tests(T, [test(H) | Acc]).

assert_equal(Expected, Value) -> ?assertEqual(Expected, Value).
assert(Expr) -> ?assert(Expr).

%%
%% internal functions
%%

with_call(undefined) -> true;
with_call({Fun, FunExpected}) when is_function(FunExpected) ->
  {module, Module} = erlang:fun_info(Fun, module),
  {name, Name} = erlang:fun_info(Fun, name),
  {arity, Arity} = erlang:fun_info(Fun, arity),

  with_call(Module, Name, Arity, FunExpected);

with_call({Fun, Opts}) when is_list(Opts) ->
  {module, Module} = erlang:fun_info(Fun, module),
  {name, Name} = erlang:fun_info(Fun, name),
  {arity, Arity} = erlang:fun_info(Fun, arity),

  ParametersShouldBe = proplists:get_value(parameter_should_be, Opts, undefined),
  WithReturn = proplists:get_value(with_return, Opts, undefined),
  with_call(Module, Name, Arity, ParametersShouldBe, WithReturn).

with_call(Module, Name, _Arity, FunExpected) when is_function(FunExpected) ->
  meck:expect(Module, Name, FunExpected).
with_call(Module, Name, Arity, undefined, WithReturn) ->
  meck:expect(Module, Name, Arity, WithReturn);
with_call(Module, Name, Arity, ParametersShouldBe, undefined) ->
  F = gen_function_with_arity(Arity, ParametersShouldBe),
  meck:expect(Module, Name, F);
with_call(Module, Name, Arity, ParametersShouldBe, WithReturn) ->
  F = gen_function_with_arity(Arity, ParametersShouldBe, WithReturn),
  meck:expect(Module, Name, F).

gen_function_with_arity(Arity, ParametersShouldBe) ->
  ArityNums = lists:seq(1, Arity),
  ArityNumStr = string_list_to_string( list_join(",", ["V"++integer_to_list(A) || A <- ArityNums]) ),
  {ok, Tokens, _} = erl_scan:string("fun ("++ArityNumStr++") -> "++atom_to_list(?MODULE)++":assert_equal(Params, ["++ArityNumStr++"]) end."),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  Binding = erl_eval:add_binding('Params', ParametersShouldBe, erl_eval:new_bindings()),
  {value, F, _} = erl_eval:expr(Form, Binding),
  F.

gen_function_with_arity(Arity, ParametersShouldBe, WithReturn) ->
  ArityNums = lists:seq(1, Arity),
  ArityNumStr = string_list_to_string( list_join(",", ["V"++integer_to_list(A) || A <- ArityNums]) ),
  {ok, Tokens, _} = erl_scan:string("fun ("++ArityNumStr++") -> "++atom_to_list(?MODULE)++":assert_equal(Params, ["++ArityNumStr++"]), Return end."),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  Binding1 = erl_eval:add_binding('Params', ParametersShouldBe, erl_eval:new_bindings()),
  Binding  = erl_eval:add_binding('Return', WithReturn, Binding1),
  {value, F, _} = erl_eval:expr(Form, Binding),
  F.

list_join(Sep, L) -> list_join(Sep, L, []).
list_join(_Sep, [], L) -> lists:reverse(L);
list_join(Sep, [H | []], L) -> list_join(Sep, [], [H | L]);
list_join(Sep, [H|T], L) -> list_join(Sep, T, [Sep, H | L]).

string_list_to_string(L) -> string_list_to_string(L, []).
string_list_to_string([], Acc) -> Acc;
string_list_to_string([H|T], Acc) -> H++string_list_to_string(T, Acc).