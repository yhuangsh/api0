-module(jsonres).

-export([success/3, success/4,
         failure/3, failure/4]).

%%====================================================================
%% API
%%====================================================================

success(M, R, S) -> result(true, M, R, S).
success(M, More, R, S) -> result(true, M, More, R, S).
failure(M, R, S) -> result(false, M, R, S).
failure(M, More, R, S) -> result(false, M, More, R, S).

%%====================================================================
%% Internal functions
%%====================================================================

result(TF, M, R, S) -> result(TF, M, #{}, R, S).
result(TF, M, More, R, S) -> 
    Output = maps:merge(#{<<"success">> => TF, <<"message">> => M}, More),
    jsx:encode(Output, R, S).