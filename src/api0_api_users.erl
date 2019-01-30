-module(api0_api_users).

-export([init/2,
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         content_types_accepted/2,
         delete_resource/2]).

-export([json_acceptors/2, 
         json_providers/2]).

-define(CT_JSON, {<<"application">>, <<"json">>, '*'}).

%%====================================================================
%% Callback functions
%%====================================================================

init(R, S) -> 
    Method = cowboy_req:method(R),
    PathList = cowboy_req:path_info(R),
    {cowboy_rest, R, S#{method => Method, path_list => PathList}}.

allowed_methods(R, S) -> {[<<"GET">>, <<"POST">>, <<"DELETE">>, <<"PUT">>], R, S}.

content_types_provided(R, S) -> {[{?CT_JSON, json_providers}], R, S}.
content_types_accepted(R, S) -> {[{?CT_JSON, json_acceptors}], R, S}.

resource_exists(R, S = #{method := M, path_list := PL}) -> resource_exists(M, PL, R, S).

delete_resource(R, S) -> 'DELETE /api0/v1/users/ID'(R, S).

%%====================================================================
%% Internal functions
%%====================================================================

%% Resource Existence
resource_exists(<<"GET">>, [Id], R, S) -> 
    OldUsers = tab_user:read(Id),
    set_resource(OldUsers, [], R, S);
resource_exists(<<"POST">>, [], R0, S) -> 
    {ok, BodyIn, R1} = cowboy_req:read_body(R0),
    %io:format("resource_exists BondyIn ~p~n", [BodyIn]),
    case NewUser = tab_user:new_from_binary(BodyIn) of
        #{<<"login_id">> := LoginId} ->
            OldUsers = tab_user:read_login_id(LoginId),
            set_resource(OldUsers, [NewUser], R1, S);
        _Else -> 
            set_resource([], [undefined], R1, S)
    end;
resource_exists(<<"PUT">>, [Id], R0, S) ->
    {ok, BodyIn, R1} = cowboy_req:read_body(R0),
    NewUser = tab_user:from_binary(BodyIn),
    OldUsers = tab_user:read(Id),
    set_resource(OldUsers, [NewUser], R1, S);
resource_exists(<<"DELETE">>, [Id], R, S) -> 
    OldUsers = tab_user:read(Id),
    set_resource(OldUsers, [], R, S);
resource_exists(_, _, R, S) -> 
    set_resource([], [], R, S).

set_resource([], NewUsers, R, S) when is_list(NewUsers) -> 
    {false, R, S#{api0_old_users => [], api0_new_users => NewUsers}};
set_resource(OldUsers, NewUsers, R, S) when is_list(OldUsers) andalso is_list(NewUsers) -> 
    {true, R, S#{api0_old_users => OldUsers, api0_new_users => NewUsers}}.

%% Providers 
json_acceptors(R, S = #{method := M, path_list := PL}) -> json_acceptors(M, PL, R, S).
json_acceptors(<<"POST">>, [], R, S) -> 'POST /api0/v1/users'(R, S);
json_acceptors(<<"PUT">>, [_Id], R, S) -> 'PUT /api0/v1/users/ID'(R, S).
%% Acceptors
json_providers(R, S = #{method := M, path_list := PL}) -> json_providers(M, PL, R, S).
json_providers(<<"GET">>, [_Id], R, S) -> 'GET /api0/v1/users/ID'(R, S).

%% Create/CRUD
'POST /api0/v1/users'(R, S = #{api0_old_users := [U], api0_new_users := _}) -> {{true, mk_resource_url(U)}, R, S}; 
'POST /api0/v1/users'(R, S = #{api0_old_users := [], api0_new_users := [undefined]}) -> {false, R, S};
'POST /api0/v1/users'(R, S = #{api0_old_users := [], api0_new_users := [U]}) -> 
    {atomic, ok} = tab_user:create(U),
    {{true, mk_resource_url(U)}, R, S}.

mk_resource_url(#{<<"id">> := Id}) -> <<<<"/api0/v1/users/">>/binary, Id/binary>>.

%% Read/CRUD
'GET /api0/v1/users/ID'(R, S = #{api0_old_users := [OldUser], api0_new_users := []}) ->
    {jsx:encode(maps:merge(#{<<"success">> => true}, OldUser)), R, S}.

%% Update/CRUD
'PUT /api0/v1/users/ID'(R, S = #{api0_old_users := [], api0_new_users := _}) -> {false, R, S};
'PUT /api0/v1/users/ID'(R, S = #{api0_old_users := _, api0_new_users := []}) -> {false, R, S};
'PUT /api0/v1/users/ID'(R, S = #{api0_old_users := [OldUser], api0_new_users := [NewUser]}) -> 
    Merged = deep_map_merge(OldUser, NewUser), 
    {atomic, ok} = tab_user:update(Merged),
    {true, R, S}.

%% Delete/CRUD
'DELETE /api0/v1/users/ID'(R, S = #{api0_old_users := [OldUser], api0_new_users := []}) -> 
    #{<<"id">> := Id} = OldUser,
    {atomic, ok} = tab_user:delete(Id),
    {true, R, S}.
    
deep_map_merge(M1, M2) -> deep_map_merge(find_map_elem(M1), find_map_elem(M2), M1, M2).

deep_map_merge([], _, M1, M2) -> maps:merge(M1, M2);
deep_map_merge(_, [], M1, M2) -> maps:merge(M1, M2);
deep_map_merge(K1, K2, M1, M2) ->
    CommonMapKeys = find_common_map_keys(K1, K2),
    MergedMapAsList = [{K, deep_map_merge(maps:get(K, M1), maps:get(K, M2))} || K <- CommonMapKeys],
    MergedMap = maps:from_list(MergedMapAsList),
    M3 = maps:merge(M1, M2),
    maps:merge(M3, MergedMap).

find_map_elem(M) -> lists:filter(fun(K) -> is_map(maps:get(K, M)) end, maps:keys(M)).

find_common_map_keys(K1, K2) -> 
    S1 = sets:from_list(K1),
    S2 = sets:from_list(K2),
    S = sets:intersection([S1, S2]),
    sets:to_list(S).

