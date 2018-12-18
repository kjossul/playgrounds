-module(foldr).
-export([foldr/3]).

foldr(_, _, []) ->
  [];
foldr(F, Z, [X|[]]) ->
  F(X, Z);
foldr(F, Z, [X|XS]) ->
  F(X, foldr(F, Z, XS)).
