-module(s0_SUITE).

-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_GET_api0_v1_info_version/1,
         test_POST_api0_v1_users/1]).
 
-define(TIMEOUT, 1000).
-define(HOST, "http://127.0.0.1").
-define(PORT, "8000").

all() -> [test_GET_api0_v1_info_version, 
          test_POST_api0_v1_users].
 
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(api0),   
    Config.

end_per_suite(_Config) -> ok.

%%====================================================================
%% Tests
%%====================================================================

%% /api0/v1/version
test_GET_api0_v1_info_version(_Config) -> 
    {200, JSONOut} = 'GET'("/api0/v1/info/version"),
    #{<<"success">> := true, <<"version">> := <<"v1">>} = JSONOut.

%% POST /api0/v1/users
test_POST_api0_v1_users(Config) ->
    {ok, BodyIn} = read_file(Config, "create_test0.json"), 
    %io:format("body in ~p~n", [BodyIn]),
    {201, Headers, _} = 'POST'("/api0/v1/users", BodyIn),
    #{"location" := Location} = Headers,
    %io:format("Location: ~p~n", [Location]),
    "/api0/v1/users/" ++ IdStr = Location,
    true = uuid:is_v4(uuid:string_to_uuid(IdStr)).

%%====================================================================
%% Internal functions
%%====================================================================

read_file(Config, Filename) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok, _} = file:read_file(DataDir ++ Filename).

'GET'(Path) ->
    {ok, {{_, Code, _}, _, BodyOut}} = 
        httpc:request(get, {prefix(Path), []}, [{timeout, ?TIMEOUT}], []),
    {Code, jsx:decode(list_to_binary(BodyOut), [return_maps])}.

'POST'(Path, BodyIn) ->
    {ok, {{_, Code, _}, Headers, Body}} = 
        httpc:request(post, {prefix(Path), [], "application/json", BodyIn}, [{timeout, ?TIMEOUT}], []),
    {Code, maps:from_list(Headers), Body}.

prefix(Path) ->
    ?HOST ++ ":" ++ ?PORT ++ Path.