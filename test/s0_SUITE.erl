-module(s0_SUITE).

-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_v1_version/1,
         test_v1_create/1]).
 
-define(TIMEOUT, 1000).
-define(HOST, "http://127.0.0.1").
-define(PORT, "8000").

all() -> [test_v1_version, 
          test_v1_create].
 
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(api0),   
    Config.

end_per_suite(_Config) -> ok.

%%====================================================================
%% Tests
%%====================================================================

%% /api0/v1/version
test_v1_version(_Config) -> 
    {200, JSONOut} = req("/api0/v1/version"),
    #{<<"success">> := true, <<"version">> := <<"v1">>} = JSONOut.

%% /api0/v1/create
test_v1_create(Config) ->
    {ok, BodyIn} = read_file(Config, "create_test0.json"), 
    io:format("body in ~p~n", [BodyIn]),
    {200, JSONOut} = req("/api0/v1/create", BodyIn),
    #{<<"success">> := true} = JSONOut.

%%====================================================================
%% Internal functions
%%====================================================================

read_file(Config, Filename) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok, _} = file:read_file(DataDir ++ Filename).

req(Path) ->
    {ok, {{_, Code, _}, _, BodyOut}} = 
        httpc:request(get, {prefix(Path), []}, 
                      [{timeout, ?TIMEOUT}], []),
    {Code, jsx:decode(list_to_binary(BodyOut), [return_maps])}.

req(Path, BodyIn) ->
    {ok, {{_, Code, _}, _, BodyOut}} = 
        httpc:request(post, {prefix(Path), [], "application/json", BodyIn},
                      [{timeout, ?TIMEOUT}], []),
    %io:format("BaoyIn ~p, Out ~p~n", [BodyIn, Ret]),
    io:format("Code ~p, body out ~p~n", [Code, BodyOut]),
    {Code, jsx:decode(list_to_binary(BodyOut), [return_maps])}.

prefix(Path) ->
    ?HOST ++ ":" ++ ?PORT ++ Path.