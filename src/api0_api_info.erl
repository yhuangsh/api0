-module(api0_api_info).

-export([init/2,
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2]).

-export([json_providers/2]).

-define(CT_JSON, {<<"application">>, <<"json">>, '*'}).

%%====================================================================
%% Callback functions
%%====================================================================

init(R, S) -> 
    Method = cowboy_req:method(R),
    PathList = cowboy_req:path_info(R),
    {cowboy_rest, R, S#{method => Method, path_list => PathList}}.

allowed_methods(R, S) -> {[<<"GET">>], R, S}.

content_types_provided(R, S) -> {[{?CT_JSON, json_providers}], R, S}.

resource_exists(R, S = #{method := M, path_list := PL}) -> resource_exists(M, PL, R, S).

%%====================================================================
%% Internal functions
%%====================================================================

%% Resource Existence
resource_exists(<<"GET">>, [<<"version">>], R, S) -> {true, R, S};
resource_exists(_, _, R, S) -> {false, R, S}.

%% Providers
json_providers(R, S = #{method := M, path_list := PL}) -> json_providers(M, PL, R, S).
json_providers(<<"GET">>, [<<"version">>], R, S) -> 'GET /api0/v1/info/version'(R, S).

%% Read/CRUD
'GET /api0/v1/info/version'(R, S) -> {"{\"success\":true,\"version\":\"v1\"}", R, S}.