%%%-------------------------------------------------------------------
%%% @author Karam Kim
%%% @copyright (C) 2017, Twinny
%%% @doc
%%%
%%% @end
%%% Created : 03. 3월 2017 오후 9:26
%%%-------------------------------------------------------------------
-module(erest_response).
-author("Karam Kim").

-type status_code() :: pos_integer().
-type header() :: {atom(), bitstring()}.
-type headers() :: list(header()).

-record(response, {
  status_code :: status_code(),
  headers :: headers(),
  body :: bitstring()
}).

-type response() :: #response{}.

%% API
-export([new/0]).

-export([
  status_code/1, status_code/2,
  headers/1, headers/2,
  body/1, body/2,
  to_proplist/1]).

-spec new() -> response().
new() -> #response{}.

-spec status_code(response()) -> status_code().
status_code(Response) -> Response#response.status_code.
-spec status_code(status_code(), response()) -> response().
status_code(Code, Response) -> Response#response{status_code = Code}.

-spec headers(response()) -> headers().
headers(Response) -> Response#response.headers.
-spec headers(headers(), response()) -> response().
headers(Headers, Response) -> Response#response{headers = Headers}.

-spec body(response()) -> bitstring().
body(Response) -> Response#response.body.
-spec body(bitstring(), response()) -> response().
body(Body, Response) -> Response#response{body = Body}.

-spec to_proplist(response()) -> list().
to_proplist(Response) ->
  [ {<<"status_code">>, status_code(Response)},
    {<<"headers">>, headers_to_proplist(Response)},
    {<<"body">>, body(Response)} ].

headers_to_proplist(Response) when is_record(Response, response) ->
  headers_to_proplist( headers(Response) );
headers_to_proplist([]) -> [];
headers_to_proplist([{HeaderName, HeaderContent} | T]) when is_list(HeaderName) ->
  [{list_to_binary(HeaderName), HeaderContent}] ++ headers_to_proplist(T).