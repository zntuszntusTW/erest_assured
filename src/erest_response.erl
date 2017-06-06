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
  time_duration :: integer(), %% milliseconds
  status_code :: status_code(),
  headers :: headers(),
  body :: bitstring()
}).

-type response() :: #response{}.

%% API
-export([is_valid/1, new/0]).

-export([
  time_duration/1, time_duration/2,
  status_code/1, status_code/2,
  headers/1, headers/2,
  body/1, body/2, body_as_json/1,
  is_json/1,
  to_proplist/1]).

-spec is_valid(term()) -> boolean().
is_valid(Response) ->
  is_record(Response, response).

-spec new() -> response().
new() -> #response{}.

-spec time_duration(response()) -> integer().
time_duration(Response) -> Response#response.time_duration.
-spec time_duration(status_code(), response()) -> response().
time_duration(TimeDuration, Response) -> Response#response{time_duration = TimeDuration}.

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

-spec body_as_json(response()) -> list().
body_as_json(Response) ->
  jsx:decode(body(Response)).

-spec is_json(response()) -> boolean().
is_json(Response) ->
  try jsx:decode(body(Response)) of
    _ -> true
  catch
    _:badarg -> false
  end.

-spec to_proplist(response()) -> list().
to_proplist({error, _}) ->
  [{error, "unreachable"}];
to_proplist(Response) ->
  Body =
    case is_json(Response) of
      true -> body_as_json(Response);
      false -> body(Response)
    end,
  [ {status_code, status_code(Response)},
    {headers, headers_to_proplist(Response)},
    {time_duration, time_duration(Response)},
    {body, Body} ].

headers_to_proplist(Response) when is_record(Response, response) ->
  headers_to_proplist( headers(Response) );
headers_to_proplist([]) -> [];
headers_to_proplist([{HeaderName, HeaderContent} | T]) when is_list(HeaderName) ->
  [{list_to_binary(HeaderName), HeaderContent}] ++ headers_to_proplist(T).