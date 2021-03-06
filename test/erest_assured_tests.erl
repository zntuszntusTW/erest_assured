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
-define(HOST, "my.test.domain").
-define(PORT, 6060).
-define(PATH, "/test/api").
-define(ParamKey, test).
-define(ParamValue, <<"value">>).
-define(Body, <<"my erest_assured body">>).
-define(JSONBody, <<"{\"key\": \"abcd\"}">>).
-define(JSONInteger, <<"{\"key\": 123}">>).
-define(JSONFloat, <<"{\"key\": 123.4}">>).
-define(JSONNested, <<"{\"key\": {\"nested\": 123}}">>).
-define(WrongBody, <<"my wrong erest_assured body">>).
-define(Describe, <<"my erest_assured describe">>).


%%==============================================================================
%% Test generator functions
%%==============================================================================


erest_assured_test_() ->
  etest:foreach(fun start/0, fun stop/1, [
    fun requester_tests/1,
    fun giver_tests/1,
    fun tester_tests/1,
    fun getter_testes/1,
    fun erest_assured_tests/1
  ]).


%%==============================================================================
%% Setup functions
%%==============================================================================


start() ->
  etest:mock_load(erest_request, [passthrough]).

stop(_) ->
  etest:mock_unload(erest_request).


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
  RequestParam = #request{parameters = {parameters, [{?ParamKey, ?ParamValue}]} },
  RequestHost  = #request{host = ?HOST },
  RequestPort  = #request{port = ?PORT },

  etest:tests([
    {?LINE, [
      {describe, "generate parameter giver"},
      {run, ?_f( (erest_assured:given( [erest_assured:param(?ParamKey, ?ParamValue)] ))(#request{}) )},
      {should_be, RequestParam}
    ]},
    {?LINE, [
      {describe, "generate host giver"},
      {run, ?_f( (erest_assured:given( [erest_assured:host(?HOST)] ))(#request{}) )},
      {should_be, RequestHost}
    ]},
    {?LINE, [
      {describe, "generate port giver"},
      {run, ?_f( (erest_assured:given( [erest_assured:port(?PORT)] ))(#request{}) )},
      {should_be, RequestPort}
    ]}
  ]).

tester_tests(_) ->
  Response0 = erest_response:new(),
  Response1 = erest_response:status_code(200, Response0),
  Response2 = erest_response:time_duration(2500, Response1),
  Response  = erest_response:body(?Body, Response2),
  ResponseJSON  = erest_response:body(?JSONBody, Response0),
  ResponseJSONInteger  = erest_response:body(?JSONInteger, Response0),
  ResponseJSONFloat  = erest_response:body(?JSONFloat, Response0),
  ResponseJSONNested  = erest_response:body(?JSONNested, Response0),
  ResponseError = {error, {}},

  etest:tests([
    {?LINE, [
      {describe, "generate status code equal_to right tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:status_code(erest_assured:equal_to(200))] ))(?Describe, Response) )},
      {should_be, {ok, {?Describe, "status code should be equal to expected value", 200, 200}, Response} }
    ]},
    {?LINE, [
      {describe, "generate status code equal_to wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:status_code(erest_assured:equal_to(500))] ))(?Describe, Response) )},
      {should_be, {fail, {?Describe, "status code should be equal to expected value", 500, 200}, Response} }
    ]},
    {?LINE, [
      {describe, "generate status code greater than right tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:status_code(erest_assured:greater_than(199))] ))(?Describe, Response) )},
      {should_be, {ok, {?Describe, "status code should be greater than expected value", 199, 200}, Response} }
    ]},
    {?LINE, [
      {describe, "generate status code greater than wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:status_code(erest_assured:greater_than(200))] ))(?Describe, Response) )},
      {should_be, {fail, {?Describe, "status code should be greater than expected value", 200, 200}, Response} }
    ]},
    {?LINE, [
      {describe, "generate status code less than right tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:status_code(erest_assured:less_than(201))] ))(?Describe, Response) )},
      {should_be, {ok, {?Describe, "status code should be less than expected value", 201, 200}, Response} }
    ]},
    {?LINE, [
      {describe, "generate status code less than wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:status_code(erest_assured:less_than(200))] ))(?Describe, Response) )},
      {should_be, {fail, {?Describe, "status code should be less than expected value", 200, 200}, Response} }
    ]},
    {?LINE, [
      {describe, "generate time duration less than 3000ms"},
      {run, ?_f( (erest_assured:then( [erest_assured:time(erest_assured:less_than(3000))] ))(?Describe, Response) )},
      {should_be, {ok, {?Describe, "time duration should be less than expected value", 3000, 2500}, Response} }
    ]},
    {?LINE, [
      {describe, "generate time duration less than wrong number"},
      {run, ?_f( (erest_assured:then( [erest_assured:time(erest_assured:less_than(2000))] ))(?Describe, Response) )},
      {should_be, {fail, {?Describe, "time duration should be less than expected value", 2000, 2500}, Response} }
    ]},
    {?LINE, [
      {describe, "generate body equal_to right tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:body(erest_assured:equal_to(?Body))] ))(?Describe, Response) )},
      {should_be, {ok, {?Describe, "body should be equal to expected value", ?Body, ?Body}, Response} }
    ]},
    {?LINE, [
      {describe, "generate body equal_to wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:body(erest_assured:equal_to(?WrongBody))] ))(?Describe, Response) )},
      {should_be, {fail, {?Describe, "body should be equal to expected value", ?WrongBody, ?Body}, Response}}
    ]},
    {?LINE, [
      {describe, "generate is_json tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:body(erest_assured:is_json())] ))(?Describe, ResponseJSON) )},
      {should_be, {ok, {?Describe, "body should be JSON", ?JSONBody}, ResponseJSON}}
    ]},
    {?LINE, [
      {describe, "generate is_json wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:body(erest_assured:is_json())] ))(?Describe, Response) )},
      {should_be, {fail, {?Describe, "body should be JSON", ?Body}, Response}}
    ]},
    {?LINE, [
      {describe, "generate json equal_to right tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:equal_to("key", <<"abcd">>))] ))(?Describe, ResponseJSON) )},
      {should_be, {ok, {?Describe, "key should be equal to expected value", <<"abcd">>, <<"abcd">>}, ResponseJSON}}
    ]},
    {?LINE, [
      {describe, "generate json equal_to wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:equal_to("key", <<"my_value">>))] ))(?Describe, ResponseJSON) )},
      {should_be, {fail, {?Describe, "key should be equal to expected value", <<"my_value">>, <<"abcd">>}, ResponseJSON}}
    ]},
    {?LINE, [
      {describe, "generate json greater_than right tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:greater_than("key", 122))] ))(?Describe, ResponseJSONInteger) )},
      {should_be, {ok, {?Describe, "key should be greater than expected value", 122, 123}, ResponseJSONInteger}}
    ]},
    {?LINE, [
      {describe, "generate json greater_than wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:greater_than("key", 123))] ))(?Describe, ResponseJSONInteger) )},
      {should_be, {fail, {?Describe, "key should be greater than expected value", 123, 123}, ResponseJSONInteger}}
    ]},
    {?LINE, [
      {describe, "generate json greater_than_or_equal_to right tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:greater_than_or_equal_to("key", 123))] ))(?Describe, ResponseJSONInteger) )},
      {should_be, {ok, {?Describe, "key should be greater than or equal to expected value", 123, 123}, ResponseJSONInteger}}
    ]},
    {?LINE, [
      {describe, "generate json greater_than_or_equal_to wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:greater_than_or_equal_to("key", 124))] ))(?Describe, ResponseJSONInteger) )},
      {should_be, {fail, {?Describe, "key should be greater than or equal to expected value", 124, 123}, ResponseJSONInteger}}
    ]},
    {?LINE, [
      {describe, "generate json less_than right tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:less_than("key", 124))] ))(?Describe, ResponseJSONInteger) )},
      {should_be, {ok, {?Describe, "key should be less than expected value", 124, 123}, ResponseJSONInteger}}
    ]},
    {?LINE, [
      {describe, "generate json less_than wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:less_than("key", 123))] ))(?Describe, ResponseJSONInteger) )},
      {should_be, {fail, {?Describe, "key should be less than expected value", 123, 123}, ResponseJSONInteger}}
    ]},
    {?LINE, [
      {describe, "generate json less_than_or_equal_to right tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:less_than_or_equal_to("key", 123))] ))(?Describe, ResponseJSONInteger) )},
      {should_be, {ok, {?Describe, "key should be less than or equal to expected value", 123, 123}, ResponseJSONInteger}}
    ]},
    {?LINE, [
      {describe, "generate json less_than_or_equal_to wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:less_than_or_equal_to("key", 122))] ))(?Describe, ResponseJSONInteger) )},
      {should_be, {fail, {?Describe, "key should be less than or equal to expected value", 122, 123}, ResponseJSONInteger}}
    ]},
    {?LINE, [
      {describe, "generate json should_be_string right tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:should_be_string("key"))] ))(?Describe, ResponseJSON) )},
      {should_be, {ok, {?Describe, "key should be string", <<"abcd">>}, ResponseJSON}}
    ]},
    {?LINE, [
      {describe, "generate json should_be_string wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:should_be_string("key"))] ))(?Describe, ResponseJSONInteger) )},
      {should_be, {fail, {?Describe, "key should be string", 123}, ResponseJSONInteger}}
    ]},
    {?LINE, [
      {describe, "generate json should_be_integer right tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:should_be_integer("key"))] ))(?Describe, ResponseJSONInteger) )},
      {should_be, {ok, {?Describe, "key should be integer", 123}, ResponseJSONInteger}}
    ]},
    {?LINE, [
      {describe, "generate json should_be_integer wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:should_be_integer("key"))] ))(?Describe, ResponseJSONFloat) )},
      {should_be, {fail, {?Describe, "key should be integer", 123.4}, ResponseJSONFloat}}
    ]},
    {?LINE, [
      {describe, "generate json should_be_float right tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:should_be_float("key"))] ))(?Describe, ResponseJSONFloat) )},
      {should_be, {ok, {?Describe, "key should be float", 123.4}, ResponseJSONFloat}}
    ]},
    {?LINE, [
      {describe, "generate json should_be_float wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:should_be_float("key"))] ))(?Describe, ResponseJSONInteger) )},
      {should_be, {fail, {?Describe, "key should be float", 123}, ResponseJSONInteger}}
    ]},
    {?LINE, [
      {describe, "generate json should_be_number right when integer tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:should_be_number("key"))] ))(?Describe, ResponseJSONInteger) )},
      {should_be, {ok, {?Describe, "key should be number", 123}, ResponseJSONInteger}}
    ]},
    {?LINE, [
      {describe, "generate json should_be_number right when float tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:should_be_number("key"))] ))(?Describe, ResponseJSONFloat) )},
      {should_be, {ok, {?Describe, "key should be number", 123.4}, ResponseJSONFloat}}
    ]},
    {?LINE, [
      {describe, "generate json should_be_number wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:should_be_number("key"))] ))(?Describe, ResponseJSON) )},
      {should_be, {fail, {?Describe, "key should be number", <<"abcd">>}, ResponseJSON}}
    ]},
    {?LINE, [
      {describe, "generate json has_key right tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:has_key("key"))] ))(?Describe, ResponseJSON) )},
      {should_be, {ok, {?Describe, "key should be exist"}, ResponseJSON}}
    ]},
    {?LINE, [
      {describe, "generate json has_key wrong tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:has_key("mykey"))] ))(?Describe, ResponseJSON) )},
      {should_be, {fail, {?Describe, "mykey should be exist"}, ResponseJSON}}
    ]},
    {?LINE, [
      {describe, "generate json has_key nested key tester"},
      {run, ?_f( (erest_assured:then( [erest_assured:json(erest_assured:has_key("key.nested"))] ))(?Describe, ResponseJSONNested) )},
      {should_be, {ok, {?Describe, "key.nested should be exist"}, ResponseJSONNested}}
    ]},
    {?LINE, [
      {describe, "generate invalid response"},
      {run, ?_f( (erest_assured:then( [erest_assured:status_code(erest_assured:equal_to(200))] ))(?Describe, ResponseError) )},
      {should_be, {fail, {?Describe, "unreachable"}, ResponseError}}
    ]}
  ]).

getter_testes(_) ->
  Response0 = erest_response:new(),
  Response  = erest_response:body(?Body, Response0),
  ResponseJSON  = erest_response:body(?JSONBody, Response0),

  etest:tests([
    {?LINE, [
      {describe, "get_body getter"},
      {run, ?_f( erest_assured:get_body(Response) )},
      {should_be, ?Body}
    ]},
    {?LINE, [
      {describe, "get_json getter"},
      {run, ?_f( erest_assured:get_json(ResponseJSON) )},
      {should_be, [{<<"key">>, <<"abcd">>}]}
    ]},
    {?LINE, [
      {describe, "get_value_from_json getter"},
      {run, ?_f( erest_assured:get_value_from_json("key", ResponseJSON) )},
      {should_be, <<"abcd">>}
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
      {describe, "erest_assured assure"},
      {run, ?_f( erest_assured:assured(?Describe, Given, Requester, Then) )},
      {with_call,
        { fun erest_request:execute/1,
          [ {parameter_should_be, [Request]},
            {with_return, Response }] }},
      {should_be, {ok, {?Describe, "body should be equal to expected value", ?Body, ?Body}, Response}}
    ]}
  ]).