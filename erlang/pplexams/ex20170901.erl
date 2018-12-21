-module(ex20170901).
-compile(export_all).

% Note: this is actually not so good because in multiple runs previous workers stay alive
parfind(XSS, X) ->
  Ids = [spawn(?MODULE, slave, [self(), XS, X]) || XS <- XSS],
  master_loop(Ids).


master_loop([]) ->
  false;

master_loop(Ids) ->
  receive
    {_, true, L} ->
      L;
    {From, false, _} ->
      master_loop([Id || Id <- Ids, Id /= From])
  end.

slave(Master, XS, X) ->
  slave(Master, XS, X, XS).

slave(Master, [], _, L) ->
  Master ! {self(), false, L};
slave(Master, [X|XS], V, L) ->
  if 
    X =:= V ->
      Master ! {self(), true, L};
    true ->
      slave(Master, XS, V, L)
  end.
