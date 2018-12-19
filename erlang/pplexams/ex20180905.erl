-module(ex20180905).
-export([create_pipe/1, send/1, send/2]).  % note that we NEED to export send/1 here

create_pipe(XS) ->
  create_pipe(XS, self()).

create_pipe([], End) ->
  process_flag(trap_exit, true), % needed to gain control of terminal
  End;
create_pipe([X|XS], Next) ->
  Pid = spawn(ex20180905, send, [Next]),
  register(X, Pid),
  create_pipe(XS, Pid).

send(D) ->
  receive
    kill ->
      D ! kill;
    Msg ->
      io:format("I am process ~w and I received message ~w~n", [self(), Msg]),
      D ! Msg,
      send(D)
  end.

send(Msg, Dest) ->
  Dest ! Msg.
