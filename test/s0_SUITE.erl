-module(s0_SUITE).

-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_GET_api0_v1_info_version/1,
         test_POST_api0_v1_users/1,
         test_GET_api0_v1_users_ID/1,
         test_PUT_api0_v1_users_ID/1]).
 
-define(TIMEOUT, 1000).
-define(HOST, "http://127.0.0.1").
-define(PORT, "8000").

all() -> [test_GET_api0_v1_info_version, 
          test_POST_api0_v1_users,
          test_GET_api0_v1_users_ID,
          test_PUT_api0_v1_users_ID].
 
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
    {200, _, JSONOut} = 'GET'("/api0/v1/info/version"),
    #{<<"success">> := true, <<"version">> := <<"v1">>} = JSONOut.

%% POST /api0/v1/users | Create/CRUD
test_POST_api0_v1_users(Config) ->
    {ok, BodyIn} = read_file(Config, "create_test0.json"), 
    %io:format("body in ~p~n", [BodyIn]),
    {201, Headers, _} = 'POST'("/api0/v1/users", BodyIn),
    {_, Id} = get_loc_id(Headers),
    %io:format("POST Location: ~p~n", [Location]),
    true = uuid:is_v4(uuid:string_to_uuid(Id)),
    %% create with same login_id a second time
    {303, Headers2, _} = 'POST'("/api0/v1/users", BodyIn),
    #{"location" := Location} = Headers2,
    IdStr = binary_to_list(Id),
    "/api0/v1/users/" ++ IdStr = Location.

%% GET /api0/v1/users/ID | Read/CRUD
test_GET_api0_v1_users_ID(Config) ->
    {ok, BodyIn} = read_file(Config, "create_test1.json"), 
    {201, Headers, _} = 'POST'("/api0/v1/users", BodyIn),
    {Location, Id} = get_loc_id(Headers),
    %io:format("GET Location: ~p~n", [Location]),
    {200, _, JSONOut} = 'GET'(Location),
    %io:format("Id: ~p~n GET JSONOut: ~p~n", [Id, JSONOut]),
    #{<<"success">> := true, 
      <<"id">> := Id, 
      <<"login_id">> := <<"user1@company.com">>,
      <<"login_type">> := <<"email">>,
      <<"more">> := #{ <<"gender">> := <<"male">>, <<"age">> := 36}} = JSONOut,
    %% read non-exists
    {404, _, _} = 'GET'("/api0/v1/users/0123456789abcdef0123456789ABCDEF").

%% PUT /api0/v1/users/ID | Update/CRUD
test_PUT_api0_v1_users_ID(Config) ->
    {ok, BodyIn} = read_file(Config, "create_test2.json"), 
    {201, Headers, _} = 'POST'("/api0/v1/users", BodyIn),
    {Location, Id} = get_loc_id(Headers),
    io:format("GET Location: ~p~n", [Location]),
    OldUser = json_decode(BodyIn),
    NewUser = OldUser#{<<"more">> => #{ <<"state">> => <<"new york">>, <<"firstname">> => <<"sarah">>}},
    BodyIn2 = json_encode(NewUser),
    io:format("BodyIn ~p~n BodyIn2 ~p~n", [BodyIn, BodyIn2]),
    {204, _, _} = 'PUT'(Location, BodyIn2),
    {200, _, JSONOut} = 'GET'(Location),
    #{<<"success">> := true, 
      <<"id">> := Id, 
      <<"login_id">> := <<"user2@apple.com">>,
      <<"login_type">> := <<"email">>,
      <<"more">> := #{<<"state">> := <<"new york">>,
                      <<"firstname">> := <<"sarah">>, 
                      <<"salary">> := 6600}} = JSONOut.

%%====================================================================
%% Internal functions
%%====================================================================

read_file(Config, Filename) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok, _} = file:read_file(DataDir ++ Filename).

'POST'(Path, BodyIn) ->
    {ok, {{_, Code, _}, Headers, Body}} = 
        httpc:request(post, {prefix(Path), [], "application/json", BodyIn}, 
            [{timeout, ?TIMEOUT}, {autoredirect, false}], []),
    {Code, maps:from_list(Headers), Body}.

'GET'(Path) ->
    {ok, {{_, Code, _}, Headers, BodyOut}} = 
        httpc:request(get, {prefix(Path), []}, 
            [{timeout, ?TIMEOUT}, {autoredirect, false}], []),
    {Code, Headers, json_decode(BodyOut)}.

'PUT'(Path, BodyIn) ->
    {ok, {{_, Code, _}, Headers, Body}} = 
        httpc:request(put, {prefix(Path), [], "application/json", BodyIn}, 
            [{timeout, ?TIMEOUT}, {autoredirect, false}], []),
    {Code, maps:from_list(Headers), Body}.

prefix(Path) ->
    ?HOST ++ ":" ++ ?PORT ++ Path.

get_loc_id(Headers) ->
    #{"location" := Location} = Headers,
    "/api0/v1/users/" ++ IdStr = Location,
    Id = list_to_binary(IdStr),
    {Location, Id}.
    
json_decode(L) when is_list(L) -> json_decode(list_to_binary(L));
json_decode(<<>>) -> #{};
json_decode(B) when is_binary(B) -> jsx:decode(B, [return_maps]).

json_encode(B) when is_map(B) -> jsx:encode(B).