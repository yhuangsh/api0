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
    {401, _, _} = 'POST'("/api0/v1/users", BodyIn),
    {403, _, _} = 'POST'("/api0/v1/users", ['x-agw-context: owner'("0123456789")], BodyIn),
    {201, Headers, _} = 'POST'("/api0/v1/users", ['x-agw-context: admin'()], BodyIn),
    {_, Id} = get_loc_id(Headers),
    %io:format("POST Location: ~p~n", [Location]),
    true = uuid:is_v4(uuid:string_to_uuid(Id)),
    %% create with same login_id a second time
    {303, Headers2, _} = 'POST'("/api0/v1/users", ['x-agw-context: admin'()], BodyIn),
    #{"location" := Location} = Headers2,
    IdStr = binary_to_list(Id),
    "/api0/v1/users/" ++ IdStr = Location.

%% GET /api0/v1/users/ID | Read/CRUD
test_GET_api0_v1_users_ID(Config) ->
    {ok, BodyIn} = read_file(Config, "create_test1.json"), 
    {201, Headers, _} = 'POST'("/api0/v1/users", ['x-agw-context: admin'()], BodyIn),
    {Location, Id} = get_loc_id(Headers),
    %io:format("GET Location: ~p~n", [Location]),
    {200, _, JSONOut} = 'GET'(Location, ['x-agw-context: owner'(Id)]),
    {200, _, JSONOut} = 'GET'(Location, ['x-agw-context: admin'()]),
    %io:format("Id: ~p~n GET JSONOut: ~p~n", [Id, JSONOut]),
    #{<<"success">> := true, 
      <<"id">> := Id, 
      <<"login_id">> := <<"user1@company.com">>,
      <<"login_type">> := <<"email">>,
      <<"more">> := #{ <<"gender">> := <<"male">>, <<"age">> := 36}} = JSONOut,
    {400, _, _} = 'GET'("/api0/v1/users", ['x-agw-context: admin'()]),
    {401, _, _} = 'GET'("/api0/v1/users/0123456789abcdef0123456789ABCDEF"),
    {403, _, _} = 'GET'("/api0/v1/users/0123456789abcdef0123456789ABCDEF", ['x-agw-context: owner'("0123456789")]),
    {404, _, _} = 'GET'("/api0/v1/users/0123456789abcdef0123456789ABCDEF", ['x-agw-context: admin'()]). 

%% PUT /api0/v1/users/ID | Update/CRUD
test_PUT_api0_v1_users_ID(Config) ->
    {ok, BodyIn} = read_file(Config, "create_test2.json"), 
    {201, Headers, _} = 'POST'("/api0/v1/users", ['x-agw-context: admin'()], BodyIn),
    {Location, Id} = get_loc_id(Headers),
    %io:format("GET Location: ~p~n", [Location]),
    OldUser = json_decode(BodyIn),
    NewUser = OldUser#{<<"more">> => #{ <<"state">> => <<"new york">>, <<"firstname">> => <<"sarah">>}},
    BodyIn2 = json_encode(NewUser),
    %io:format("BodyIn ~p~n BodyIn2 ~p~n", [BodyIn, BodyIn2]),
    {204, _, _} = 'PUT'(Location, ['x-agw-context: admin'()], BodyIn2),
    {200, _, JSONOut} = 'GET'(Location, ['x-agw-context: admin'()]),
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

'x-agw-context: admin'() -> 'x-agw-context'("*", "admin").
'x-agw-context: owner'(UserId) -> 'x-agw-context'(UserId, "owner").
'x-agw-context'(UserId, Scope) when is_binary(UserId) -> 
    'x-agw-context'(binary_to_list(UserId), Scope);
'x-agw-context'(UserId, Scope) when is_list(UserId), is_list(Scope) ->
    {"x-agw-context", "user_id=" ++ UserId ++ ";scope=" ++ Scope}.

read_file(Config, Filename) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok, _} = file:read_file(DataDir ++ Filename).

'POST'(Path, BodyIn) -> 'POST'(Path, [], BodyIn).
'POST'(Path, HeadersIn, BodyIn) ->
    {ok, {{_, Code, _}, HeadersOut, Body}} = 
        httpc:request(post, {prefix(Path), HeadersIn, "application/json", BodyIn}, 
            [{timeout, ?TIMEOUT}, {autoredirect, false}], []),
    {Code, maps:from_list(HeadersOut), Body}.

'GET'(Path) -> 'GET'(Path, []).
'GET'(Path, HeadersIn) ->
    {ok, {{_, Code, _}, HeadersOut, BodyOut}} = 
        httpc:request(get, {prefix(Path), HeadersIn}, 
            [{timeout, ?TIMEOUT}, {autoredirect, false}], []),
    {Code, HeadersOut, json_decode(BodyOut)}.

'PUT'(Path, BodyIn) -> 'PUT'(Path, [], BodyIn).
'PUT'(Path, HeadersIn, BodyIn) ->
    {ok, {{_, Code, _}, HeadersOut, Body}} = 
        httpc:request(put, {prefix(Path), HeadersIn, "application/json", BodyIn}, 
            [{timeout, ?TIMEOUT}, {autoredirect, false}], []),
    {Code, maps:from_list(HeadersOut), Body}.

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