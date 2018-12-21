-module(ex20180116).
-compile(export_all).

main() ->
  X = spawn(fun() -> logger("A") end),
  [spawn(fun() -> loggee(X, Y) end) || Y <- ["A","B","A","B","A","B"]].

logger("A") ->
  receive
    {log, K, "A"} ->
      io:format("log: ~w ~p~n", [K, "A"]),
      logger("B")
  end;

logger("B") ->
  receive
    {log, K, "B"} ->
      io:format("log: ~w ~p~n", [K, "B"]),
      logger("A")
  end.

loggee(X, Y) ->
  receive
  after rand:uniform(100) ->
          X ! {log, self(), Y}
  end.
