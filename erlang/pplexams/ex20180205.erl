-module(ex20180205).
-compile(export_all).

applier(S) ->
  receive
    {F, From} ->
      case S =:= F(S) of
        true ->
          From ! S;
        false ->
          From ! nack,
          applier(F(S))
      end
  end.

fix(F, Z) ->
  A = spawn(?MODULE, applier, [Z]),
  fix_loop(A, F).

fix_loop(A, F) ->
  A ! {F, self()},
  receive
    nack ->
      fix_loop(A, F);
    R ->
      R
  end.

