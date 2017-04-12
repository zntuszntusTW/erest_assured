%%%-------------------------------------------------------------------
%%% @author Karam Kim
%%% @copyright (C) 2017, Twinny
%%% @doc
%%%
%%% @end
%%% Created : 03. 3월 2017 오전 11:09
%%%-------------------------------------------------------------------
-module(erest_request).
-author("Karam Kim").

-include("erest_request.hrl").

%% API
-export([
  execute/1, url/1
]).

-export([
  new/0, new/1,
  method/1, method/2,
  protocol/1, protocol/2,
  host/1, host/2,
  path/1, path/2,
  query_string/1, query_string/2, add_query_string/2,
  parameters/1, parameters/2, add_parameter/2,
  headers/1, headers/2, add_header/2,
  port/1, port/2,
  body/1, body/2,
  timeout/1, timeout/2,
  to_proplist/1
]).

-spec new() -> request().
new() -> #request{}.
-spec new(term()) -> request().
new(Data) ->
  R1 = new(),
  R2 = method(proplists:get_value(method, Data, undefined), R1),
  R3 = protocol(proplists:get_value(protocol, Data, undefined), R2),
  R4 = host(proplists:get_value(host, Data, undefined), R3),
  R5 = path(proplists:get_value(path, Data, undefined), R4),
  R6 = query_string(proplists:get_value(query_string, Data, undefined), R5),
  R7 = parameters(proplists:get_value(parameters, Data, undefined), R6),
  R8 = headers(proplists:get_value(headers, Data, undefined), R7),
  R8.

-spec method(request()) -> method().
method(Request) -> Request#request.method.
-spec method(method(), request()) -> request().
method(Method, Request) -> Request#request{method = Method}.

-spec protocol(request()) -> protocol().
protocol(Request) -> Request#request.protocol.
-spec protocol(protocol(), request()) -> request().
protocol(Protocol, Request) -> Request#request{protocol = Protocol}.

-spec host(request()) -> host().
host(Request) -> Request#request.host.
-spec host(host(), request()) -> request().
host(Host, Request) -> Request#request{host = Host}.

-spec path(request()) -> path().
path(Request) -> Request#request.path.
-spec path(path(), request()) -> request().
path(Path, Request) -> Request#request{path = Path}.

-spec query_string(request()) -> query_string().
query_string(Request) -> Request#request.query_string.
-spec query_string(query_string(), request()) -> request().
query_string({query_string, _Value} = QueryString, Request) -> Request#request{query_string = QueryString};
query_string(QueryString, Request) -> Request#request{query_string = to_query_string(QueryString)}.
-spec add_query_string(a_query_string(), request()) -> request().
add_query_string({K, _V} = AQueryString, Request) when is_atom(K) ->
  {query_string, Qs} = query_string(Request),
  query_string([AQueryString | Qs], Request).

-spec parameters(request()) -> parameters().
parameters(Request) -> Request#request.parameters.
-spec parameters(parameters(), request()) -> request().
parameters({parameters, _Value} = Parameters, Request) -> Request#request{parameters = Parameters};
parameters(Parameters, Request) -> Request#request{parameters = to_parameters(Parameters)}.
-spec add_parameter(parameter(), request()) -> request().
add_parameter({K, _V} = Parameter, Request) when is_atom(K) ->
  {parameters, Ps} = parameters(Request),
  parameters([Parameter | Ps], Request).

-spec headers(request()) -> headers().
headers(Request) -> Request#request.headers.
-spec headers(headers(), request()) -> request().
headers({headers, _Value} = Headers, Request) -> Request#request{headers = Headers};
headers(Headers, Request) -> Request#request{headers = to_headers(Headers)}.
-spec add_header(header(), request()) -> request().
add_header({K, _V} = Header, Request) when is_atom(K) ->
  {headers, Hs} = headers(Request),
  headers([Header | Hs], Request).

-spec port(request()) -> port_num().
port(Request) -> Request#request.port.
-spec port(port_num(), request()) -> request().
port(Port, Request) -> Request#request{port = Port}.

-spec body(request()) -> bitstring().
body(Request) -> Request#request.body.
-spec body(bitstring(), request()) -> request().
body(Body, Request) -> Request#request{body = Body}.

-spec timeout(request()) -> integer().
timeout(Request) -> Request#request.timeout.
-spec timeout(integer(), request()) -> request().
timeout(Timeout, Request) -> Request#request{timeout = Timeout}.

-spec url(request()) -> string().
url(Request) ->
  Protocol = atom_to_list(protocol(Request)),
  Host = host(Request),
  Path = path_to_string(path(Request), query_string(Request)),
  Port = integer_to_list(port(Request)),
  Protocol++"://"++Host++":"++Port++Path.


-spec isSSL(request()) -> boolean().
isSSL(#request{protocol = https}) -> true;
isSSL(_Request) -> false.

-spec execute(request()) -> {ok, response()} | {error, reason()}.
execute(Request) ->
  Response0 = erest_response:new(),
  Result = lhttpc:request(
    host(Request),
    port(Request),
    isSSL(Request),
    path_to_string(Request),
    method(Request),
    headers_to_string(headers(Request)),
    parameters_to_binary(Request),
    timeout(Request), []),

  case Result of
    {ok, {{StatusCode, _Status}, Headers, Body}} ->
      Response1 = erest_response:status_code(StatusCode, Response0),
      Response2 = erest_response:headers(Headers, Response1),
      Response  = erest_response:body(Body, Response2),
      Response;
    _ ->
      {error, Result}
  end.

-spec to_proplist(request()) -> list().
to_proplist(Request) ->
  [ {method, method(Request)},
    {host, host(Request)},
    {path, path_to_string(Request)},
    {protocol, protocol(Request)},
    {query_string, qs_to_string(query_string(Request))},
    {parameters,parameters_to_binary(Request)},
    {headers, headers_to_string(headers(Request))},
    {port, port(Request)},
    {body, body(Request)},
    {timeout, timeout(Request)} ].

%%
%% internal function
%%

parameters_to_binary(#request{method = get} = _Request) -> <<"">>;
parameters_to_binary(#request{body = <<"">>} = Request) -> parameters_to_binary(parameters(Request));
parameters_to_binary(#request{} = Request) -> body(Request);
parameters_to_binary({parameters, []}) -> build_parameters([]);
parameters_to_binary({parameters, [{K, _} | _] = L}) when is_atom(K) -> build_parameters(L).
qs_to_string({query_string, []}) -> build_query_string([]);
qs_to_string({query_string, [{K, _} | _] = L}) when is_atom(K) -> build_query_string(L).
headers_to_string({headers, []}) -> build_headers([]);
headers_to_string({headers, [{K, _} | _] = L}) when is_atom(K) -> build_headers(L).


path_to_string(#request{method = get, parameters = {parameters, [{_, _}|_]}} = Request) ->
  path_to_string(path(Request), query_string(Request)) ++ "&" ++ binary_to_list(parameters_to_binary(parameters(Request)));
path_to_string(#request{} = Request) ->
  path_to_string(path(Request), query_string(Request));
path_to_string(undefined) -> "/";
path_to_string([$/ | _] = Path) -> Path;
path_to_string(Path) when is_list(Path) -> [$/ | Path].
path_to_string(Path, {query_string, []}) -> path_to_string(Path);
path_to_string(Path, {query_string, [{K, _} | _]} = Qs) when is_atom(K) ->
  path_to_string(Path) ++ "?" ++ qs_to_string(Qs).

to_query_string(Value) when is_list(Value) -> {query_string, Value}.
to_parameters(Value) when is_list(Value) -> {parameters, Value}.
to_headers(Value) when is_list(Value) -> {headers, Value}.

build_query_string([]) -> "";
build_query_string([{K, _V}=Query | []]) when is_atom(K) ->
  build_query_string(Query);
build_query_string([{K, _V}=Query | T]) when is_atom(K) ->
  build_query_string(Query) ++ "&" ++ build_query_string(T);
build_query_string({K, V}) when is_atom(K) ->
  atom_to_list(K) ++ "=" ++ V.

build_headers([]) -> "";
build_headers([{K, _V}=Header | []]) when is_atom(K) ->
  build_headers(Header);
build_headers([{K, _V}=Header | T]) when is_atom(K) ->
  build_headers(Header) ++ "\n\r" ++ build_headers(T);
build_headers({K, V}) when is_atom(K) ->
  atom_to_list(K) ++ ": " ++ V.

build_parameters([]) -> <<"">>;
build_parameters([{K, _V}=Query | []]) when is_atom(K) ->
  build_parameters(Query);
build_parameters([{K, _V}=Query | T]) when is_atom(K) ->
  Param1 = build_parameters(Query),
  Param2 = build_parameters(T),
  <<Param1/binary, "&", Param2/binary>>;
build_parameters({K, V}) when is_atom(K) ->
  Key = atom_to_binary(K,utf8),
  <<Key/binary, "=", V/binary>>.