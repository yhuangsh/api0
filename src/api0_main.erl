-module(api0_main).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_cast/2, handle_call/3]).

-define(MNESIA_DATA, "/deploy/api0/mnesia-data").
-define(USER_TAB, user).

%%====================================================================
%% APIs
%%====================================================================

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, state0(), []).

%%====================================================================
%% Callbacks
%%====================================================================

init(S) ->
    Np = connect_prev_node(node()),
    ok = mnesia:start(),
    {atomic, ok} = start_mnesia(Np),
    start_cowboy(S), 
    {ok, S}.

handle_call(_Cmd, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Cmd, State) ->
    {noreply, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% TODO: only works for api0-x, where 0 <= x <= 9
connect_prev_node(N) when is_atom(N) -> connect_prev_node(atom_to_list(N));
connect_prev_node([$a,$p,$p,$@,$a,$p,$i,$0,$-, N | _]) -> connect_prev_node(N);
connect_prev_node($0) -> none;
connect_prev_node(N) when is_integer(N) ->    
    Np0 = ["app@api0-", N-1, ".api0.default.svc.cluster.local"],
    Np1 = list_to_atom(lists:flatten(Np0)),
    io:format("~npinging [~p]~n", [Np1]),
    pong = net_adm:ping(Np1),
    Np1.

%%
start_mnesia(none) -> {atomic, ok} = mnesia:create_table(?USER_TAB, []);
start_mnesia(Np) when is_atom(Np) ->
    {ok, _} = mnesia:change_config(extra_db_nodes, [Np]),
    {atomic, ok} = mnesia:add_table_copy(?USER_TAB, node(), ram_copies).

%%
start_cowboy(S) ->
    Dispatch = cowboy_router:compile(routes(S)),
    {ok, _} = cowboy:start_clear(api0_listner, [{port, 8000}], #{env => #{dispatch => Dispatch}}).

routes(S) -> [route0(S)].
route0(S) -> {'_', [{prefix("/probes/:pb"), api0_probes, S},
                    {prefix("/v1/version"), api0_api_version, S},
                    {'_', api0_api_404, []}]}.                

prefix(Path) -> application:get_env(api0, prefix, "") ++ Path.

state0() -> #{}.

