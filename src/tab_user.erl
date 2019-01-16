-module(tab_user).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([create_table/0,
         add_table_copy/0,

         create/1,
         read/1,
         update/1,
         delete/1]).

-record(user, {id, firstname, lastname, gender, age, email, primary_phone, extra}).

-define(TAB_USER, user).

%%====================================================================
%% API
%%====================================================================

create_table() -> 
    mnesia:create_table(?TAB_USER, 
                        [{attributes, record_info(fields, ?TAB_USER)},
                         {disc_copies, [node()]}]).
add_table_copy() ->
    mnesia:add_table_copy(?TAB_USER, node(), disc_copies).

%%
create(U) -> mnesia:transaction(fun() -> mnesia:write(U) end). 
read(Id) -> mnesia:dirty_read(?TAB_USER, Id).
update(U = #user{id = Id}) -> 
    mnesia:transaction(
        fun() ->
            case mnesia:read(?TAB_USER, Id) of 
                [_] -> mnesia:write(U);
                Else -> Else
            end
        end).
delete(id) -> mnesia:transaction(fun() -> mnesia:delete({?TAB_USER, id}) end).

%%====================================================================
%% Unit tests
%%====================================================================

-ifdef(TEST).

crud_test() -> 
    mnesia:delete_schema([node()]),
    ?assertEqual(ok, mnesia:create_schema([node()])),
    ?assertEqual(ok, mnesia:start()),
    ?assertEqual({atomic, ok}, create_table()),
    U0 = #user{id = u0000, firstname = "yong", lastname = "huang", gender = "male"},
    U1 = #user{id = u0001},
    U0new = #user{id = u0000, firstname = "jiayu", lastname = "cheng", gender = "female"},
    ?assertEqual({atomic, ok}, create(U0)),
    ?assertEqual([U0], read(u0000)),
    ?assertEqual([], read(u0001)),
    ?assertEqual({atomic, ok}, update(U0)),
    ?assertEqual({atomic, []}, update(U1)),
    ?assertEqual({atomic, ok}, update(U0new)),
    ?assertEqual([U0new], read(u0000)),
    ?assertEqual({atomic, ok}, mnesia:delete_table(?TAB_USER)),
    ?assertEqual(stopped, mnesia:stop()).

-endif.