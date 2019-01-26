-module(api0_api_create).

-export([init/2,
         allowed_methods/2,
         content_types_accepted/2]).

%%
-export([cmd_create/2]).

init(R, S) -> {cowboy_rest, R, S}.

allowed_methods(R, S) -> {[<<"POST">>], R, S}.

content_types_accepted(R, S) ->
    Accepted = [{{<<"application">>, <<"json">>, '*'}, cmd_create}], 
	{Accepted, R, S}.

cmd_create(R0, S) -> 
    {ok, Body, R1} = read_body(R0),
    cmd_create_1(jsx:is_json(Body), Body, R1, S).

cmd_create_1(true, Body, R, S) -> 
    JSONList = lists:map(fun({K, V}) -> {binary_to_atom(K, latin1), V} end, jsx:decode(Body)),
    cmd_create_2(maps:from_list(JSONList), R, S);
cmd_create_1(false, _, R, S) -> jsonres:failure("invalid request data", R, S).

cmd_create_2(JSONMap = #{login_id := LoginId, login_type := LoginType}, R, S) -> 
    io:format("JSONMap is ~p~n", [JSONMap]),
    JSONMap2 = JSONMap#{login_type := binary_to_atom(LoginType, latin1)},
    More = maps:filter(fun(login_id, _) -> true; 
                          (login_type, _) -> true end, JSONMap2),
    cmd_create_3(LoginId, LoginType, More, R, S);
cmd_create_2(JSONMap, R0, S) -> 
    io:format("JSONMap2 is ~p~n", [JSONMap]),
    {BodyOut, R1, S} = jsonres:failure(<<"unsupported format">>, R0, S),
    R2 = cowboy_req:set_resp_header(<<"content-type">>, "application/json", R1),
    R3 = cowboy_req:set_resp_body(BodyOut, R2),
    {true, R3, S}. 
    
cmd_create_3(LoginId, email, More, R, S) -> cmd_create_4(LoginId, email, More, R, S);
cmd_create_3(LoginId, phone, More, R, S) -> cmd_create_4(LoginId, phone, More, R, S);
cmd_create_3(_, _, _, R0, S) -> 
    {BodyOut, R1, S} = jsonres:failure(<<"invalid login type">>, R0, S),
    R2 = cowboy_req:set_resp_header(<<"content-type">>, "application/json", R1),
    R3 = cowboy_req:set_resp_body(BodyOut, R2),
    {true, R3, S}.

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
