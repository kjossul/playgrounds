-module(ex20170705).
-compile(export_all).

leafy(V) ->
  receive
    {ask, P} ->
      P ! {self(), V}
  end.

branchy(F, L, R) ->
  receive
    {ask, P} ->
      L ! {ask, self()},
      R ! {ask, self()},
      receive
        {L, VL} ->
          ok
      end,
      receive
        {R, VR} ->
          V = F(VL, VR),
          P ! {self(), V}
      end
  end.

activate({leaf, V}, _) ->
  spawn(?MODULE, leafy, [V]);
activate({branch, L, R}, F) ->
  Lid = activate(L, F),
  Rid = activate(R, F),
  spawn(?MODULE, branchy, [F, Lid, Rid]).

test() ->
  T1 = {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}},
  A1 = activate(T1, fun min/2),
  A1 ! {ask, self()},
  receive
    {A1, V} -> V
  end.

