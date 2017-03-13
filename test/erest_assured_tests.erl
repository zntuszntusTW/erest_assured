%%%-------------------------------------------------------------------
%%% @author Karam Kim
%%% @copyright (C) 2017, Twinny
%%% @doc
%%%
%%% @end
%%% Created : 13. 3월 2017 오후 3:10
%%%-------------------------------------------------------------------
-module(erest_assured_tests).
-author("Karam Kim").

-include_lib("eunit/include/eunit.hrl").
-include("../src/erest_request.hrl").

-define(_f(F), fun() -> F end).
-define(PATH, "/test/api").
-define(ParamKey, test).
-define(ParamValue, <<"value">>).
-define(Body, <<"my erest_assured body">>).
-define(WrongBody, <<"my wrong erest_assured body">>).
-define(Describe, <<"my erest_assured describe">>).

%%==============================================================================
%% Test generator functions
%%==============================================================================


erest_assured_test_() ->
  {foreach, fun start/0, fun stop/1, [
    fun requester_tests/1,
    fun giver_tests/1,
    fun tester_tests/1,
    fun erest_assured_tests/1
  ]}.


%%==============================================================================
%% Setup functions
%%==============================================================================


start() ->
  meck:new(erest_request, [passthrough]).

stop(_) ->
  meck:unload(erest_request).


%%==============================================================================
%% Test functions
%%==============================================================================


requester_tests(_) ->
  Response = erest_response:new(),

  etest:tests([
    {?LINE, [
      {describe, "generate http get method requester"},
      {run, ?_f( (erest_assured:get(?PATH))(#request{}) )},
      {with_call,
        { fun erest_request:execute/1,
          [ {parameter_should_be, [#request{method = get, path = ?PATH}]},
            {with_return, Response }] }},
      {should_be, Response}
    ]},
    {?LINE, [
      {describe, "generate http post method requester"},
      {run, ?_f( (erest_assured:post(?PATH))(#request{}) )},
      {with_call,
        { fun erest_request:execute/1,
          [ {parameter_should_be, [#request{method = post, path = ?PATH}]},
            {with_return, Response }] }},
      {should_be, Response}
    ]},
    {?LINE, [
      {describe, "generate http put method requester"},
      {run, ?_f( (erest_assured:put(?PATH))(#request{}) )},
      {with_call,
        { fun erest_request:execute/1,
          [ {parameter_should_be, [#request{method = put, path = ?PATH}]},
            {with_return, Response }] }},
      {should_be, Response}
    ]},
    {?LINE, [
      {describe, "generate http patch method requester"},
      {run, ?_f( (erest_assured:patch(?PATH))(#request{}) )},
      {with_call,
        { fun erest_request:execute/1,
          [ {parameter_should_be, [#request{method = patch, path = ?PATH}]},
            {with_return, Response }] }},
      {should_be, Response}
    ]},
    {?LINE, [
      {describe, "generate http delete method requester"},
      {run, ?_f( (erest_assured:delete(?PATH))(#request{}) )},
      {with_call,
        { fun erest_request:execute/1,
          [ {parameter_should_be, [#request{method = delete, path = ?PATH}]},
            {with_return, Response }] }},
      {should_be, Response}
    ]},
    {?LINE, [
      {describe, "generate http option method requester"},
      {run, ?_f( (erest_assured:option(?PATH))(#request{}) )},
      {with_call,
        { fun erest_request:execute/1,
          [ {parameter_should_be, [#request{method = option, path = ?PATH}]},
            {with_return, Response }] }},
      {should_be, Response}
    ]},
    {?LINE, [
      {describe, "generate requester"},
      {run, ?_f( (erest_assured:request( erest_assured:get(?PATH) ))(#request{}) )},
      {with_call,
        { fun erest_request:execute/1,
          [ {parameter_should_be, [#request{method = get, path = ?PATH}]},
            {with_return, Response }] }},
      {should_be, Response}
    ]}
  ]).


giver_tests(_) ->
  Response0 = erest_response:new(),
  Response  = erest_response:body(?Body, Response0),
  Request = #request{parameters = {parameters, [{?ParamKey, ?ParamValue}]} },

  etest:tests([
    {?LINE, [
      {describe, "generate parameter giver"},
      {run, ?_f( (erest_assured:given( [erest_assured:param(?ParamKey, ?ParamValue)] ))(#request{}) )},
      {with_call,
        { fun erest_request:execute/1,
          [ {parameter_should_be, [Request]},
            {with_return, Response }] }},
      {should_be, Request}
    ]}
  ]).

tester_tests(_) ->
  Response0 = erest_response:new(),
  Response  = erest_response:body(?Body, Response0),

  etest:tests([
    {?LINE, [
      {describe, "generate body equal_to right tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:body(erest_assured:equal_to(?Body))] ))(?Describe, Response) )},
      {should_be, {ok, {?Describe, "should be equal", ?Body, ?Body}}}
    ]},
    {?LINE, [
      {describe, "generate body equal_to wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:body(erest_assured:equal_to(?WrongBody))] ))(?Describe, Response) )},
      {should_be, {fail, {?Describe, "should be equal", ?WrongBody, ?Body}}}
    ]}
  ]).

erest_assured_tests(_) ->
  Response0 = erest_response:new(),
  Response  = erest_response:body(?Body, Response0),

  Request = #request{parameters = {parameters, [{?ParamKey, ?ParamValue}]}, method = get, path = ?PATH },

  Given = erest_assured:given( [erest_assured:param(?ParamKey, ?ParamValue)] ),
  Requester = erest_assured:request( erest_assured:get(?PATH) ),
  Then = erest_assured:then( [erest_assured:body(erest_assured:equal_to(?Body))] ),

  etest:tests([
    {?LINE, [
      {describe, "erest_assured assure test"},
      {run, ?_f( erest_assured:assured(?Describe, Given, Requester, Then) )},
      {with_call,
        { fun erest_request:execute/1,
          [ {parameter_should_be, [Request]},
            {with_return, Response }] }},
      {should_be, {ok, {?Describe, "should be equal", ?Body, ?Body}}}
    ]}
  ]).