-module(httpres).

-export(['200'/2, '200'/3, '200'/4,
         '400'/2,
         '404'/2, '404'/3, '404'/4]).

-define(CT_TEXT_TEXT, <<"text/text">>).
-define(CT_APPLICATION_JSON, <<"application/json">>).

%%====================================================================
%% API
%%====================================================================

'200'(R, S) -> reply(200, R, S).
'200'(T, R, S) -> reply(200, T, R, S).
'200'(json, T, R, S) -> reply(200, ?CT_APPLICATION_JSON, T, R, S).

'400'(R, S) -> reply(400, R, S).
'404'(R, S) -> reply(404, R, S).
'404'(T, R, S) -> reply(404, T, R, S).
'404'(json, T, R, S) -> reply(404, ?CT_APPLICATION_JSON, T, R, S).

%%====================================================================
%% Internal functions
%%====================================================================

reply(C, R, S) -> {ok, cowboy_req:reply(C, R), S}.
reply(C, T, R, S) -> reply(C, ?CT_TEXT_TEXT, T, R, S).
reply(C, CT, T, R, S) -> {ok, cowboy_req:reply(C, #{<<"content-type">> => CT}, T, R), S}.