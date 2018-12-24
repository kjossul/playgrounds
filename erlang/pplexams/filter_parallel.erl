-module(filter_parallel).
-compile(export_all).

loop(F, X, M) ->
  case F(X) of
    true  -> M ! {X, self()};
    false -> M ! {null, self()}
  end.
    
master_loop(XS, []) ->
  XS;
master_loop(XS, [P|PS]) ->
  receive
    {null, P} -> master_loop(XS, PS);
    {X, P}    -> master_loop(XS ++ [X], PS)
  end.

filter(F, XS) ->
  PS = [spawn(?MODULE, loop, [F, X, self()]) || X <- XS],
  master_loop([], PS).
