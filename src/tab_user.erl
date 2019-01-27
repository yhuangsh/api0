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
         from_proplist/1,

         new/3,
         new_from_binary/1,
         new_from_proplist/1]).

-record(user, {id, login_id, login_type, more}).

-define(TAB_USER, user).

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
create(U = #user{id = Id}) when is_map(U) -> 
    mnesia:transaction(
        fun() -> 
            case mnesia:read(?TAB_USER, Id) of
                [] -> mnesia:write(U);
                [_] -> exists
            end
        end).
read(Id) when is_binary(Id) -> 
    mnesia:dirty_read(?TAB_USER, Id).
read_login_id(LoginId) when is_binary(LoginId) -> 
    mnesia:dirty_index_read(?TAB_USER, LoginId, #user.login_id).
update(U = #user{id = Id}) when is_map(U) -> 
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
from_binary(B) when is_binary(B) ->
    case jsx:is_json(B) of
        false -> #{};
        true -> 
            P = lists:map(fun({K, V}) -> {binary_to_atom(K, latin1), V} end, jsx:decode(B)),
            from_proplist(P)   
    end.  

from_proplist(P) when is_list(P) ->
    Id = proplists:get_value(id, P), 
    LoginId = proplists:get_value(login_id, P),
    LoginType = proplists:get_value(login_type, P),
    More = proplists:get_value(more, P, #{}),
    case Id =:= undefined orelse LoginId =:= undefined orelse LoginType =:= undefined of
        true -> #{};
        false -> 
            #user{
                id = Id,
                login_id = LoginId,
                login_type = LoginType,
                more = More}
    end.

%% 
new(LoginId, LoginType, More) when is_binary(LoginId) andalso is_binary(LoginType) andalso is_map(More) ->
    from_proplist([{id, gen_id()}, {login_id, LoginId}, {login_type, LoginType}, {more, More}]).

new_from_binary(B) when is_binary(B) ->
    case jsx:is_json(B) of
        false -> #{};
        true -> 
            P = lists:map(fun({K, V}) -> {binary_to_atom(K, latin1), V} end, jsx:decode(B)),
            new_from_proplist(P)
    end.
            
new_from_proplist(P) when is_list(P) -> from_proplist([{id, gen_id()} | P]).

gen_id() -> crypto:strong_rand_bytes(32).

%%====================================================================
%% Unit tests
%%====================================================================

-ifdef(TEST).

crud_test_() ->
    {setup, fun setup/0, fun cleanup/1, 
     fun (D) ->
        [test_create(D),
         test_read(D),
         test_update(D),
         test_delete(D)] 
     end}.

setup() ->
    mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {atomic, ok} = create_table(),
    U0 = #user{id = <<"u0000">>, login_id = <<"u0@x.com">>, login_type = <<"email">>},
    U1 = #user{id = <<"u0001">>},
    U0new = #user{id = <<"u0000">>, login_id = <<"13812345678">>, login_type = <<"phone">>},
    {U0, U1, U0new}.

cleanup(_) -> 
    ?assertEqual({atomic, ok}, mnesia:delete_table(?TAB_USER)),
    ?assertEqual(stopped, mnesia:stop()).

test_create({U0, _U1, _U0new}) ->    
    [?_assertEqual({atomic, ok}, create(U0)),
     ?_assertEqual({atomic, exists}, create(U0))].

test_read({U0, _U1, _U0new}) ->
    [?_assertEqual([U0], read(<<"u0000">>)),
     ?_assertEqual([U0], read_login_id("u0@x.com")),
     ?_assertEqual([], read(<<"u0001">>)),
     ?_assertEqual([], read_login_id("another@y.com"))].

test_update({U0, U1, U0new}) ->
    [?_assertEqual({atomic, ok}, update(U0)),
     ?_assertEqual({atomic, not_exist}, update(U1)),
     ?_assertEqual({atomic, ok}, update(U0new)),
     ?_assertEqual([U0new], read(<<"u0000">>)),
     ?_assertEqual([U0new], read_login_id("13812345678"))].

test_delete({_U0, _U1, _U0new}) ->
    [?_assertEqual({atomic, ok}, delete(<<"u0000">>)),
     ?_assertEqual([], read(<<"u0000">>)),
     ?_assertEqual([], read_login_id("u0@x.com"))].

-endif.