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
    case jsx:is_json(Body) of
        true -> create_user(jsx:decode(Body), R1, S);
        false -> jsonres:failure("invalid request data", R1, S)
    end.

create_user(Data, R, S) ->
    U = tab_user:from_proplist(Data),
    case tab_user:create(U) of
        {atomic, ok} -> jsonres:success(<<"user created">>, R, S);
        {atomic, exists} -> jsonres:failure(<<"user exists">>, R, S)
    end.

read_body(R) -> read_body(R, <<"">>).
read_body(R0, Acc) ->
    case cowboy_req:read_body(R0) of
        {ok, Data, R1} -> {ok, <<Acc/binary, Data/binary >>, R1};
        {more, Data, R1} -> read_body(R1, <<Acc/binary, Data/binary >>)
    end.
