%%%-------------------------------------------------------------------
%%% @author Karam Kim
%%% @copyright (C) 2017, Twinny
%%% @doc
%%%
%%% @end
%%% Created : 06. 3월 2017 오후 7:45
%%%-------------------------------------------------------------------
-module(erest_request_tests).
-author("Karam Kim").

-include_lib("eunit/include/eunit.hrl").

-define(_erest_request_test(T), {?LINE, T}).
-define(_f(F), fun() -> F end).

-define(HOST, "my.api.domain").
-define(PORT, 80).
-define(PROTOCOL, http).
-define(METHOD, post).
-define(PATH, "/api/path").
-define(QS, [{qs1, "qs1_value"}, {qs2, "qs2_value"}]).
-define(PATH_QS, "/api/path?qs1=qs1_value&qs2=qs2_value").
-define(PARAMETERS, [{param1, <<"param1_value">>}, {param2, <<"param2_value">>}]).
-define(PARAMETERS_OUT, <<"param1=param1_value&param2=param2_value">>).
-define(PATH_QS_PARAM, "/api/path?qs1=qs1_value&qs2=qs2_value&param1=param1_value&param2=param2_value").
-define(HEADERS, [{header1, "header1_value"}, {header2, "header2_value"}]).
-define(HEADERS_OUT, "header1: header1_value\n\rheader2: header2_value").
-define(BODY, <<"request body">>).
-define(TIMEOUT, 5000).
-define(CONFIG, []).


%%==============================================================================
%% Test generator functions
%%==============================================================================


erest_request_test_() ->
  etest:foreach(
    fun start/0,
    fun stop/1,
    [
      fun execute_input_tests/1,
      fun execute_output_tests/1
    ]
  ).


%%==============================================================================
%% Setup functions
%%==============================================================================


start() ->
  etest:mock_load(lhttpc).

stop(_) ->
  etest:mock_unload(lhttpc).


%%==============================================================================
%% Test functions
%%==============================================================================


execute_input_tests(_) ->
  Request0 = erest_request:new(),
  Request1 = erest_request:method(?METHOD, Request0),
  Request2 = erest_request:port(?PORT, Request1),
  Request3 = erest_request:host(?HOST, Request2),
  Request4 = erest_request:path(?PATH, Request3),
  Request5 = erest_request:parameters(?PARAMETERS, Request4),
  Request6 = erest_request:headers(?HEADERS, Request5),
  Request7 = erest_request:body(?BODY, Request6),
  RequestHTTP = erest_request:protocol(http, Request7),
  RequestHTTPS = erest_request:protocol(https, Request7),
  RequestQS = erest_request:query_string(?QS, RequestHTTP),
  RequestParams = erest_request:protocol(http, Request6),
  RequestGET =  erest_request:body(<<"">>, erest_request:method(get, RequestQS)),
  RequestPOST = erest_request:body(<<"">>, erest_request:method(post, RequestQS)),

  etest:tests([
    {?LINE, [
      {describe, "execute http input test"},
      {run, ?_f(erest_request:execute(RequestHTTP))},
      {with_call,
        { fun lhttpc:request/9,
          [ {parameter_should_be, [?HOST, ?PORT, false, ?PATH, ?METHOD, ?HEADERS_OUT, ?BODY, ?TIMEOUT, []]},
            {with_return, {ok, {{200, "OK"}, [], ?BODY}} }]}}
    ]},
    {?LINE, [
      {describe, "execute https input test"},
      {run, ?_f(erest_request:execute(RequestHTTPS))},
      {with_call,
        { fun lhttpc:request/9,
          [ {parameter_should_be, [?HOST, ?PORT, true, ?PATH, ?METHOD, ?HEADERS_OUT, ?BODY, ?TIMEOUT, []]},
            {with_return, {ok, {{200, "OK"}, [], ?BODY}} }]}}
    ]},
    {?LINE, [
      {describe, "execute http input test with parameters"},
      {run, ?_f(erest_request:execute(RequestParams))},
      {with_call,
        { fun lhttpc:request/9,
          [ {parameter_should_be, [?HOST, ?PORT, false, ?PATH, ?METHOD, ?HEADERS_OUT, ?PARAMETERS_OUT, ?TIMEOUT, []]},
            {with_return, {ok, {{200, "OK"}, [], ?BODY}} }]}}
    ]},
    {?LINE, [
      {describe, "execute http input test with query_string"},
      {run, ?_f(erest_request:execute(RequestQS))},
      {with_call,
        { fun lhttpc:request/9,
          [ {parameter_should_be, [?HOST, ?PORT, false, ?PATH_QS, ?METHOD, ?HEADERS_OUT, ?BODY, ?TIMEOUT, []]},
            {with_return, {ok, {{200, "OK"}, [], ?BODY}} }]}}
    ]},
    {?LINE, [
      {describe, "execute http input test with get request"},
      {run, ?_f(erest_request:execute(RequestGET))},
      {with_call,
        { fun lhttpc:request/9,
          [ {parameter_should_be, [?HOST, ?PORT, false, ?PATH_QS_PARAM, get, ?HEADERS_OUT, <<"">>, ?TIMEOUT, []]},
            {with_return, {ok, {{200, "OK"}, [], ?BODY}} }]}}
    ]},
    {?LINE, [
      {describe, "execute http input test with post request"},
      {run, ?_f(erest_request:execute(RequestPOST))},
      {with_call,
        { fun lhttpc:request/9,
          [ {parameter_should_be, [?HOST, ?PORT, false, ?PATH_QS, post, ?HEADERS_OUT, ?PARAMETERS_OUT, ?TIMEOUT, []]},
            {with_return, {ok, {{200, "OK"}, [], ?BODY}} }]}}
    ]}
  ]).

execute_output_tests(_) ->
  RequestHTTP = erest_request:new(),

  Response0 = erest_response:new(),
  Response1 = erest_response:headers([], Response0),
  Response2 = erest_response:body(?BODY, Response1),
  Response3 = erest_response:time_duration(0, Response2),
  Response200 = erest_response:status_code(200, Response3),
  Response404 = erest_response:status_code(404, Response3),

  etest:tests([
    {?LINE, [
      {describe, "execute status code 200 output test"},
      {run, ?_f(erest_request:execute(RequestHTTP)) },
      {with_call,
        { fun lhttpc:request/9,
          [{with_return, {ok, {{200, "OK"}, [], ?BODY}} }] }},
      {should_be, Response200}
    ]},
    {?LINE, [
      {describe, "execute status code 404 output test"},
      {run, ?_f(erest_request:execute(RequestHTTP)) },
      {with_call,
        { fun lhttpc:request/9,
          [{with_return, {ok, {{404, "OK"}, [], ?BODY}} }] }},
      {should_be, Response404}
    ]}
  ]).

%%==============================================================================
%% Internal functions
%%==============================================================================
