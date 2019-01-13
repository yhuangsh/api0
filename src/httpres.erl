-module(httpres).

-export(['200'/2, '200'/3,
         '400'/3, '404'/3]).

-define(CT_TEXT_TEXT, <<"text/text">>).
-define(CT_TEXT_JSON, <<"text/json">>).

%%====================================================================
%% API
%%====================================================================

'200'(R, S) -> reply(200, R, S).
'200'(T, R, S) -> reply(200, T, R, S).

'400'(T, R, S) -> reply(400, T, R, S).
'404'(T, R, S) -> reply(404, T, R, S).

%%====================================================================
%% Internal functions
%%====================================================================

reply(C, R, S) -> {ok, cowboy_req:reply(C, R), S}.
reply(C, T, R, S) -> reply(C, ?CT_TEXT_JSON, T, R, S).
reply(C, CT, T, R, S) -> {ok, cowbot_req:reply(C, #{<<"content-type">> => CT}, T, R), S}.