%%%-------------------------------------------------------------------
%%% @author Karam Kim
%%% @copyright (C) 2017, Twinny
%%% @doc
%%%
%%% @end
%%% Created : 14. 3월 2017 오후 9:13
%%%-------------------------------------------------------------------
-module(json_path_tests).
-author("Karam Kim").

-include_lib("eunit/include/eunit.hrl").

-define(_f(F), fun() -> F end).


%%==============================================================================
%% Test generator functions
%%==============================================================================


json_path_test_() ->
  [ fun search_tests/0 ].


%%==============================================================================
%% Test functions
%%==============================================================================


search_tests() ->
  JSON = jsx:decode(<<"{
      \"year\": \"2017\",
      \"store\": {
          \"book\": [
              {
                  \"category\": \"reference\",
                  \"author\": \"Nigel Rees\",
                  \"title\": \"Sayings of the Century\",
                  \"price\": 8.95
              },
              {
                  \"category\": \"fiction\",
                  \"author\": \"Evelyn Waugh\",
                  \"title\": \"Sword of Honour\",
                  \"price\": 12.99
              },
              {
                  \"category\": \"fiction\",
                  \"author\": \"Herman Melville\",
                  \"title\": \"Moby Dick\",
                  \"isbn\": \"0-553-21311-3\",
                  \"price\": 8.99
              },
              {
                  \"category\": \"fiction\",
                  \"author\": \"J. R. R. Tolkien\",
                  \"title\": \"The Lord of the Rings\",
                  \"isbn\": \"0-395-19395-8\",
                  \"price\": 22.99
              }
          ],
          \"bicycle\": {
              \"color\": \"red\",
              \"price\": 19.95
          }
      },
      \"expensive\": 10,
      \"genre\": [\"reference\", \"fiction\", \"poem\"]
  }">>),

  etest:tests([
    {?LINE, [
      {describe, "search year"},
      {run, ?_f( json_path:search("year", JSON) )},
      {should_be, <<"2017">>}
    ]},
    {?LINE, [
      {describe, "search store.bicycle.price"},
      {run, ?_f( json_path:search("store.bicycle.price", JSON) )},
      {should_be, 19.95}
    ]},
    {?LINE, [
      {describe, "search store.book[0].author"},
      {run, ?_f( json_path:search("store.book[0].author", JSON) )},
      {should_be, <<"Nigel Rees">>}
    ]},
    {?LINE, [
      {describe, "search genre[1,2]"},
      {run, ?_f( json_path:search("genre[1,2]", JSON) )},
      {should_be, [<<"fiction">>, <<"poem">>]}
    ]}
  ]).