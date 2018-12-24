-module(map_parallel).
-export([map/2, runit/3]).

runit(Proc, F, X) -> Proc ! F(X).

map(F, L) ->
  W = lists:map(fun(X) -> spawn(?MODULE, runit, [self(), F, X]) end, L),
  lists:map(fun (P) ->
                receive
                  V -> V
                end
            end, W).

