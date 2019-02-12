-module(api0_api_users).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/2,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         forbidden/2,
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
    Context = cowboy_req:header(<<"x-agw-context">>, R, undefined),
    %io:format("api_user:init M = ~p~n, PL = ~p~n, C = ~p~n", [Method, PathList, Context]),
    {cowboy_rest, R, S#{method => Method, path_list => PathList, context => Context}}.

allowed_methods(R, S) -> {[<<"GET">>, <<"POST">>, <<"DELETE">>, <<"PUT">>], R, S}.

malformed_request(R, S = #{method := M, path_list := PL}) -> malformed_request(M, PL, R, S).

is_authorized(R, S = #{context := C}) -> is_authorized(C, R, S).

forbidden(R, S = #{method := M, path_list := PL, context := C}) -> forbidden(M, PL, C, R, S).

content_types_provided(R, S) -> {[{?CT_JSON, json_providers}], R, S}.
content_types_accepted(R, S) -> {[{?CT_JSON, json_acceptors}], R, S}.

resource_exists(R, S = #{method := M, path_list := PL}) -> resource_exists(M, PL, R, S).

delete_resource(R, S) -> 'DELETE /api0/v1/users/ID'(R, S).

%%====================================================================
%% Internal functions
%%====================================================================

%% parse api gateway context
%% in format: a=b;c=d;...;last_k=last_v
%% out: #{a => b, c => d, ..., last_k => last_v
%% where a, c, last_k converted into atoms while b, d, last_v stay unchanged as binary 
parse_context(B) ->
    ParamList = string:split(B, ";"),
    ParamPropList = 
        lists:map(
            fun(E) -> 
                [K, V] = string:split(E, "="), 
                {binary_to_atom(K, latin1),  V} 
            end, 
            ParamList),
    maps:from_list(ParamPropList).

malformed_request(<<"GET">>, [], R, S) -> {tre, R, S};
malformed_request(_, _, R, S) -> {false, R, S}.

%% Not authorized if Context is undefined
is_authorized(undefined, R, S) -> {{false, 'www-authenticate'()}, R, S};
is_authorized(C, R, S) -> {true, R, S#{context => parse_context(C)}}.

'www-authenticate'() -> <<"Bearer realm=\"api0\"">>.

%% Forbidden 
forbidden(<<"GET">>, [Id1], #{user_id := Id2, scope := Scope}, R, S) -> 
    {not_admin(Scope) andalso not_owner(Id1, Id2), R, S}; 
forbidden(<<"POST">>, [], #{scope := Scope}, R, S) -> {not_admin(Scope), R, S}; 
forbidden(<<"PUT">>, [Id1], #{user_id := Id2, scope := Scope}, R, S) -> 
    {not_admin(Scope) andalso not_owner(Id1, Id2), R, S};
forbidden(<<"DELETE">>, [_], #{scope := Scope}, R, S) -> {not_admin(Scope), R, S};
forbidden(_, _, _, R, S) -> {false, R, S}.

not_admin(Scope) -> Scope =/= <<"admin">>.
not_owner(TargetId, SourceId) -> TargetId =/= SourceId.

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
    NewUser = jsx:decode(BodyIn, [return_maps]),
    OldUsers = tab_user:read(Id),
    set_resource(OldUsers, [NewUser], R1, S);
resource_exists(<<"DELETE">>, [Id], R, S) -> 
    OldUsers = tab_user:read(Id),
    set_resource(OldUsers, [], R, S);
resource_exists(_, _, R, S) -> 
    set_resource([], [], R, S).

set_resource([], NewUsers, R, S) when is_list(NewUsers) -> 
    {false, R, S#{old_user => [], new_user => NewUsers}};
set_resource(OldUsers, NewUsers, R, S) when is_list(OldUsers) andalso is_list(NewUsers) -> 
    {true, R, S#{old_user => OldUsers, new_user => NewUsers}}.

%% Acceptors
json_acceptors(R, S = #{method := M, path_list := PL}) -> json_acceptors(M, PL, R, S).
json_acceptors(<<"POST">>, [], R, S) -> 'POST /api0/v1/users'(R, S);
json_acceptors(<<"PUT">>, [_Id], R, S) -> 'PUT /api0/v1/users/ID'(R, S).
%% Providers 
json_providers(R, S = #{method := M, path_list := PL}) -> json_providers(M, PL, R, S).
json_providers(<<"GET">>, [_Id], R, S) -> 'GET /api0/v1/users/ID'(R, S).

%% Create/CRUD
'POST /api0/v1/users'(R, S = #{old_user := [U], new_user := _}) -> {{true, mk_resource_url(U)}, R, S}; 
'POST /api0/v1/users'(R, S = #{old_user := [], new_user := [undefined]}) -> {false, R, S};
'POST /api0/v1/users'(R, S = #{old_user := [], new_user := [U]}) -> 
    {atomic, ok} = tab_user:create(U),
    {{true, mk_resource_url(U)}, R, S}.

mk_resource_url(#{<<"id">> := Id}) -> <<<<"/api0/v1/users/">>/binary, Id/binary>>.

%% Read/CRUD
'GET /api0/v1/users/ID'(R, S = #{old_user := [OldUser], new_user := []}) ->
    {jsx:encode(maps:merge(#{<<"success">> => true}, OldUser)), R, S}.

%% Update/CRUD
'PUT /api0/v1/users/ID'(R, S = #{old_user := [], new_user := _}) -> {false, R, S};
'PUT /api0/v1/users/ID'(R, S = #{old_user := _, new_user := []}) -> {false, R, S};
'PUT /api0/v1/users/ID'(R, S = #{old_user := [OldUser], new_user := [NewUser]}) -> 
    case tab_user:is_mergeable(NewUser) of
        false -> {false, R, S};
        true ->
            UpdateUser = tab_user:merge(OldUser, NewUser), 
            {atomic, ok} = tab_user:update(UpdateUser), 
            {true, R, S}
    end.

%% Delete/CRUD
'DELETE /api0/v1/users/ID'(R, S = #{old_user := [OldUser], new_user := []}) -> 
    #{<<"id">> := Id} = OldUser,
    {atomic, ok} = tab_user:delete(Id),
    {true, R, S}.

%%====================================================================
%% Unit tests
%%====================================================================


-ifdef(TEST).

parse_context_test() ->
    C1 = parse_context(<<"k1=v1;k2=v2">>),
    ?assertEqual(#{k1 => <<"v1">>, k2 => <<"v2">>}, C1).

-endif.