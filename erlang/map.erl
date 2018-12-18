-module(map).
-export([map/2]).

mp(_, [], Acc) ->
  Acc;

mp(F, [X|XS], Acc) ->
  mp(F, XS, [Acc|F(X)]).

map(F, XS) ->
  mp(F, XS, []).

