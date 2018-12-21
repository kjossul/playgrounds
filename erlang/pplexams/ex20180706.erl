-module(ex20180706).
-compile(export_all).

buffer(Content) ->
  receive
    {get, From} ->
      if
        Content =:= [] -> 
          From ! empty,
          buffer(Content);
        true ->
          [H|T] = Content,  % problem: this could be empty here
          From ! H,
          buffer(T)
      end;
    {put, Data} ->
      buffer(Content ++ [Data])
  end.

producer(From, To, Buffer, Father) ->
  if
    From < To ->
      Buffer ! {put, From},
      io:format("~w produced ~p~n", [self(), From]),
      producer(From+1, To, Buffer, Father);
    true -> 
      Father ! {self(), done}
  end.

consumer(Buffer) ->
  Buffer ! {get, self()},
  receive
    done -> ok;
    V ->
      io:format("~w consumed ~p~n", [self(), V]),
      consumer(Buffer)
  end.

main() ->
  B = spawn_link(?MODULE, buffer, [[]]),
  P1 = spawn(?MODULE, producer, [1,10,B, self()]),
  C1 = spawn_link(?MODULE, consumer, [B]),
  C2 = spawn_link(?MODULE, consumer, [B]),
  receive
    {P1, done} ->
      exit(die)
  end.
