-module(ex20170720).
-compile(export_all).

loop(V) ->
  receive
    {set, X} ->
      loop(X);
    {get, Sender} ->
      Sender ! V,
      loop(V)
  end.

create_dlist(0) ->
  [];
create_dlist(N) ->
  [spawn(?MODULE, loop, [0])|create_dlist(N-1)].

dlist_to_list([]) ->
  [];
dlist_to_list([X|XS]) ->
  X ! {get, self()},
  receive
    V ->
      [V | dlist_to_list(XS)]
  end.

send_update(_, _, []) ->
  [];
send_update(F, [V|VS], [X|XS]) ->
  X ! {set, F(V)},
  send_update(F, VS, XS).

map(F, XS) ->
  L = dlist_to_list(XS),
  send_update(F, L, XS),
  ok.
