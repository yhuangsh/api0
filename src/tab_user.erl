-module(tab_user).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([create_table/0,
         add_table_copy/0,

         create/1,
         read/1, 
         read_login_id/1,
         update/1,
         delete/1,
         
         from_binary/1, 
         new_from_binary/1]).

-record(user, {id, login_id, login_type, more}).

-define(TAB_USER, user).
-define(ID_LEN, 16).

%%====================================================================
%% API
%%====================================================================

%% Bootstrap
create_table() -> 
    mnesia:create_table(?TAB_USER, 
                        [{attributes, record_info(fields, ?TAB_USER)},
                         {index, [login_id]},
                         {disc_copies, [node()]}]).
add_table_copy() ->
    mnesia:add_table_copy(?TAB_USER, node(), disc_copies).

%% CRUD
create(U) when is_map(U) -> create(map2rec(U));
create(U = #user{id = Id}) when is_tuple(U) -> 
    mnesia:transaction(
        fun() -> 
            case mnesia:read(?TAB_USER, Id) of
                [] -> mnesia:write(U);
                [_] -> exists
            end
        end).

read(Id) when is_binary(Id) -> 
    Users = mnesia:dirty_read(?TAB_USER, Id),
    lists:map(fun(U) -> rec2map(U) end, Users).
read_login_id(LoginId) when is_binary(LoginId) -> 
    Users = mnesia:dirty_index_read(?TAB_USER, LoginId, #user.login_id),
    lists:map(fun(U) -> rec2map(U) end, Users).

update(U) when is_map(U) -> update(map2rec(U));
update(U = #user{id = Id}) when is_tuple(U) -> 
    mnesia:transaction(
        fun() ->
            case mnesia:read(?TAB_USER, Id) of 
                [_] -> mnesia:write(U);
                [] -> not_exist
            end
        end).

delete(Id) when is_binary(Id) -> 
    mnesia:transaction(fun() -> mnesia:delete({?TAB_USER, Id}) end).

%%
map2rec(U = #{<<"id">> := Id, <<"login_id">> := LoginId, <<"login_type">> := LoginType, <<"more">> := More}) when is_map(U) ->
    #user{id = Id, login_id = LoginId, login_type = LoginType, more = More}.
rec2map(U = #user{id = Id, login_id = LoginId, login_type = LoginType, more = More}) when is_tuple(U) ->
    #{<<"id">> => Id, <<"login_id">> => LoginId, <<"login_type">> => LoginType, <<"more">> => More}.

%%
from_binary(B) when is_binary(B) -> validate_user(mk_user(B)).
new_from_binary(B) when is_binary(B) -> new_from_binary(mk_user(B));
new_from_binary({undefined, LoginId, LoginType, More}) -> validate_user({gen_id(), LoginId, LoginType, More}).

mk_user(B) -> mk_user(jsx:is_json(B), B).
mk_user(false, _) -> undefined;
mk_user(true, B) ->
    U = jsx:decode(B, [return_maps]), 
    Id = maps:get(<<"id">>, U, undefined),
    LoginId = maps:get(<<"login_id">>, U, undefined),
    LoginType = maps:get(<<"login_type">>, U, undefined),
    More = maps:get(<<"more">>, U, #{}),
    {Id, LoginId, LoginType, More}.

validate_user(undefined) -> undefined;
validate_user({undefined, _, _, _}) -> undefined;
validate_user({_, undefined, _, _}) -> undefined;
validate_user({_, _, undefined, _}) -> undefined;
validate_user({Id, LoginId, LoginType, More}) -> 
    #{<<"id">> => Id, <<"login_id">> => LoginId, <<"login_type">> => LoginType, <<"more">> => More}.

gen_id() -> uuid:uuid_to_string(uuid:get_v4(), binary_nodash).
%%====================================================================
%% Unit tests
%%====================================================================

-ifdef(TEST).

crud_test_() ->
    {setup, fun setup/0, fun cleanup/1, 
     fun (D) ->
        [test_from_binary(D),
         test_new_from_binary(D),
         test_create(D),
         test_read(D),
         test_update(D),
         test_delete(D)] 
     end}.

setup() ->
    mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {atomic, ok} = create_table(),
    U0 = #{<<"id">> => <<"u0000">>, <<"login_id">> => <<"u0@x.com">>, <<"login_type">> => <<"email">>, <<"more">> => #{}},
    U1 = #{<<"id">> => <<"u0001">>, <<"login_id">> => <<"u1@y.com">>, <<"login_type">> => <<"email">>, <<"more">> => #{<<"extra">> => <<"extra data">>}},
    U0new = #{<<"id">> => <<"u0000">>, <<"login_id">> => <<"13812345678">>, <<"login_type">> => <<"phone">>, <<"more">> => #{<<"operator">> => <<"cmcc">>}},
    {U0, U1, U0new}.

cleanup(_) -> 
    ?assertEqual({atomic, ok}, mnesia:delete_table(?TAB_USER)),
    ?assertEqual(stopped, mnesia:stop()).

test_from_binary({U0, U1, U0new}) ->
    U0x = from_binary(<<"{\"id\":\"u0000\", \"login_id\":\"u0@x.com\", \"login_type\":\"email\"}">>),
    U1x = from_binary(<<"{\"id\":\"u0001\", \"login_id\":\"u1@y.com\", \"login_type\":\"email\"}">>),
    U0newx = from_binary(<<"{\"id\":\"u0000\", \"login_id\":\"13812345678\", \"login_type\":\"phone\", \"more\": { \"operator\":\"cmcc\"}}">>),
    [?_assertEqual(U0, U0x),
     ?_assertNotEqual(U1, U1x),
     ?_assertEqual(U0new, U0newx)].

test_new_from_binary(_) ->
    A = new_from_binary(<<"{\"login_id\":\"u0@x.com\", \"login_type\":\"email\"}">>),
    B = new_from_binary(<<"{\"login_id\":\"u1@y.com\", \"login_type\":\"email\"}">>),
    C = new_from_binary(<<"{\"login_id\":\"13812345678\", \"login_type\":\"phone\", \"more\": { \"operator\":\"cmcc\"}}">>),
    #{<<"id">> := IdA, <<"login_id">> := LoginIdA, <<"more">> := MoreA} = A,
    #{<<"id">> := IdB, <<"login_id">> := LoginIdB, <<"more">> := MoreB} = B,
    #{<<"id">> := IdC, <<"login_id">> := LoginIdC, <<"more">> := MoreC} = C,
    [?_assertEqual(?ID_LEN, byte_size(IdA)),
     ?_assertEqual(?ID_LEN, byte_size(IdB)),
     ?_assertEqual(?ID_LEN, byte_size(IdC)),
     ?_assertEqual(<<"u0@x.com">>, LoginIdA),
     ?_assertEqual(<<"u1@y.com">>, LoginIdB),
     ?_assertEqual(<<"13812345678">>, LoginIdC),
     ?_assertEqual(#{}, MoreA),
     ?_assertEqual(#{}, MoreB),
     ?_assertEqual(#{<<"operator">> => <<"cmcc">>}, MoreC)].

test_create({U0, _U1, _U0new}) ->    
    [?_assertEqual({atomic, ok}, create(U0)),
     ?_assertEqual({atomic, exists}, create(U0))].

test_read({U0, _U1, _U0new}) ->
    [?_assertEqual([U0], read(<<"u0000">>)),
     ?_assertEqual([U0], read_login_id(<<"u0@x.com">>)),
     ?_assertEqual([], read(<<"u0001">>)),
     ?_assertEqual([], read_login_id(<<"another@y.com">>))].

test_update({U0, U1, U0new}) ->
    [?_assertEqual({atomic, ok}, update(U0)),
     ?_assertEqual({atomic, not_exist}, update(U1)),
     ?_assertEqual({atomic, ok}, update(U0new)),
     ?_assertEqual([U0new], read(<<"u0000">>)),
     ?_assertEqual([U0new], read_login_id(<<"13812345678">>))].

test_delete({_U0, _U1, _U0new}) ->
    [?_assertEqual({atomic, ok}, delete(<<"u0000">>)),
     ?_assertEqual([], read(<<"u0000">>)),
     ?_assertEqual([], read_login_id(<<"u0@x.com">>))].

-endif.