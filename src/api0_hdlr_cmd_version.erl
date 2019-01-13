-module(web0_hdlr_cmd_version).

-export([init/2,
         allowed_method/2,
         content_type_provided/2]).

init(R, S) -> {cowboy_rest, R, S}.

allowed_method(R, S) -> {[<<"GET">>], R, S}.

content_type_provided(R, S) -> {[{{<<"text">>, <<"json">>, '*'}, cmd_version}], R, S}.


