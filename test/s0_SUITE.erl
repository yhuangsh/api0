-module(s0_SUITE).

-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_v1_version/1]).
 
-define(TIMEOUT, 1000).
-define(PORT, "8000").

all() -> [test_v1_version].
 
init_per_suite(Config) ->
    io:format("start app~n"),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(api0),   
    Config.

end_per_suite(_Config) -> ok.

test_v1_version(_Config) -> 
    {200, BodyOut} = req("/api0/v1/version"),
    io:format("response: ~p,~n", [jsx:decode(BodyOut, [return_maps])]),
    #{<<"success">> := true, <<"version">> := <<"v1">>} = jsx:decode(BodyOut, [return_maps]).
    
req(Path) ->
    io:format("requesting: ~p~n", [prefix(Path)]),
    {ok, {{_, Code, _}, _, BodyOut}} = httpc:request(get, {prefix(Path), []}, [{timeout, ?TIMEOUT}], []),
    {Code, list_to_binary(BodyOut)}.

prefix(Path) ->
    "http://127.0.0.1:" ++ ?PORT ++ Path.