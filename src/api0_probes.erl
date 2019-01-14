-module(api0_probes).

-export([init/2]).

init(R0, S0) ->
    Probe = cowboy_req:binding(pb, R0),
    handle_probe(Probe, R0, S0).

handle_probe(undefined, R, S) -> httpres:'404'(R, S);
handle_probe(<<"live">>, R, S) -> httpres:'200'(<<"ok, live">>, R, S);
handle_probe(<<"ready">>, R, S) -> httpres:'200'(<<"ok, ready">>, R, S).
