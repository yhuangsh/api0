-module(api0_api_version).

-export([init/2,
         allowed_methods/2,
         content_types_provided/2]).

%%
-export([cmd_version/2]).

init(R, S) -> {cowboy_rest, R, S}.

allowed_methods(R, S) -> {[<<"GET">>], R, S}.

content_types_provided(R, S) -> {[{{<<"application">>, <<"json">>, '*'}, cmd_version}], R, S}.

cmd_version(R, S) -> 
    Body = jsx:encode(#{<<"success">> => true, 
                        <<"version">> => <<"v1">>}),
    {Body, R, S}.                        



