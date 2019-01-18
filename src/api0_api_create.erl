-module(api0_api_create).

-export([init/2,
         allowed_methods/2,
         content_types_provided/2]).

%%
-export([cmd_create/2]).

init(R, S) -> {cowboy_rest, R, S}.

allowed_methods(R, S) -> {[<<"POST">>], R, S}.

content_types_provided(R, S) -> {[{{<<"application">>, <<"json">>, '*'}, cmd_create}], R, S}.

cmd_create(R0, S) -> 
    {ok, Body, R1} = read_body(R0),
    cmd_create_1(jsx:is_json(Body), Body, R1, S).

cmd_create_1(true, Body, R, S) -> cmd_create_2(jsx:decode(Body), R, S);
cmd_create_1(false, _, R, S) -> jsonres:failure("invalid request data", R, S).

cmd_create_2(BodyMap = #{login_id := LoginId, login_type := LoginType}, R, S) -> 
    More = maps:filter(fun(login_id, _) -> true; 
                          (login_type, _) -> true end, BodyMap),
    cmd_create_3(LoginId, LoginType, More, R, S).

cmd_create_3(LoginId, email, More, R, S) -> cmd_create_4(LoginId, email, More, R, S);
cmd_create_3(LoginId, phone, More, R, S) -> cmd_create_4(LoginId, phone, More, R, S);
cmd_create_3(_, _, _, R, S) -> jsonres:failure(<<"invalid login type">>, R, S).

cmd_create_4(LoginId, LoginType, More, R, S) -> 
    cmd_create_5(tab_user:read_login_id(LoginId), LoginId, LoginType, More, R, S).

cmd_create_5([], LoginId, LoginType, More, R, S) ->
    U = tab_user:new(LoginId, LoginType, More),
    {atomic, ok} = tab_user:create(U),
    jsonres:success(<<"user created">>, tab_user:to_map(U), R, S);
cmd_create_5([_], _, _, _, R, S) -> jsonres:failure(<<"user exists">>, R, S).

read_body(R) -> read_body(R, <<"">>).
read_body(R0, Acc) ->
    case cowboy_req:read_body(R0) of
        {ok, Data, R1} -> {ok, <<Acc/binary, Data/binary >>, R1};
        {more, Data, R1} -> read_body(R1, <<Acc/binary, Data/binary >>)
    end.
