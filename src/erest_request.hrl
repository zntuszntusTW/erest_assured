
%%%-------------------------------------------------------------------
%%% @author Karam Kim
%%% @copyright (C) 2017, Twinny
%%% @doc
%%%
%%% @end
%%% Created : 13. 3월 2017 오후 3:35
%%%-------------------------------------------------------------------
-author("Karam Kim").

-type method() :: get | post | put | option | delete | patch.
-type protocol() :: http | https.
-type host() :: string().
-type path() :: string().
-type a_query_string() :: {atom(), string()}.
-type query_string() :: {query_string, list(a_query_string())}.
-type parameter() :: {atom(), bitstring()}.
-type parameters() :: {parameters, list(parameter())}.
-type header() :: {atom(), string()}.
-type headers() :: {headers, list(header())}.
-type port_num() :: pos_integer().

-type reason() :: term().
-type response() :: erest_response:response().

-record(request, {
  method = get :: method(),
  host = undefined :: host(),
  path = undefined :: path(),
  protocol = http :: protocol(),
  query_string = {query_string, []} :: query_string(),
  parameters = {parameters, []} :: parameters(),
  headers = {headers, []} :: headers(),
  port = 80 :: port_num(),
  body = <<"">> :: bitstring(),
  timeout = 5000 :: pos_integer()
}).

-type request() :: #request{}.