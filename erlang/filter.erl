-module(filter).
-export([filter/2]).

filter(_, []) ->
  [];
filter(P, [X|XS]) ->
  case P(X) of
    true -> [X|filter(P, XS)];
    false -> filter(P, XS)
  end.
