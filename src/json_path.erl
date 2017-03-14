%%%-------------------------------------------------------------------
%%% @author Karam Kim
%%% @copyright (C) 2017, Twinny
%%% @doc
%%%
%%% @end
%%% Created : 14. 3월 2017 오후 5:53
%%%-------------------------------------------------------------------
-module(json_path).
-author("Karam Kim").

%% API
-export([search/2]).

search(Path, JSON) ->
  ParsedPath = parse_path(Path),
  search_path(ParsedPath, JSON).

search_path([], Value) -> Value;
search_path([{key, Key} | T], JSON) ->
  search_path(T, search_tuple(Key, JSON));
search_path([{index, Idx} | T], JSON) ->
  search_path(T, search_list(Idx, JSON));
search_path([{index, Idx1, Idx2} | T], JSON) ->
  search_path(T, search_list(Idx1, Idx2, JSON)).

search_list(Index1, Index2, JsonList) when is_integer(Index1) andalso is_integer(Index2) ->
  lists:sublist(JsonList, Index1+1, Index2-Index1+1).
search_list(Index, JsonList) when is_integer(Index) ->
  lists:nth(Index+1, JsonList).
search_tuple(Key, JsonTuple) ->
  proplists:get_value(Key, JsonTuple, undefined).

parse_path(Path) when is_list(Path) ->
  parse_path(list_to_binary(Path));
parse_path(Path) when is_binary(Path) ->
  Paths = lists:filter(fun(X) -> X =/= <<>> end, binary:split(Path, <<".">>, [global])),
  parse_path_tuple(Paths).

parse_path_tuple([]) -> [];
parse_path_tuple([Path | T]) ->
  {Key, List} = parse_key_list(Path),
  Paths =
    case List of
      undefined -> [{key, Key}];
      _ -> [{key, Key}] ++ parse_path_list(List)
    end,
  Paths ++ parse_path_tuple(T).

parse_path_list(List) -> [tag_path_list(L) || L <- List].
tag_path_list([Idx1, Idx2]) -> {index, Idx1, Idx2};
tag_path_list(Idx) -> {index, Idx}.

parse_key_list(Path) ->
  Paths = binary:split(Path, [<<"[">>, <<"]">>], [global]),
  [Key | Selectors] = Paths,
  {Key, [ parse_list_selector(S) || S <- lists:filter(fun(X) -> X =/= <<>> end, Selectors)] }.

parse_list_selector(Selector) ->
  Indexes = lists:filter(fun(X) -> X =/= <<>> end, binary:split(Selector, <<",">>, [global])),
  case Indexes of
    [Idx1, Idx2] -> [binary_to_integer(Idx1), binary_to_integer(Idx2)];
    [Idx] -> binary_to_integer(Idx)
  end.