-module(api0_main).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_cast/2, handle_call/3]).

%%====================================================================
%% APIs
%%====================================================================

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, state0(), []).

%%====================================================================
%% Callbacks
%%====================================================================

init(S) ->
    {ok, S}.

handle_call(_Cmd, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Cmd, State) ->
    {noreply, State}.

%%====================================================================
%% Internal functions
%%====================================================================

state0() -> #{}.
