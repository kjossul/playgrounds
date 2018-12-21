-module(ex20180720).
-compile(export_all).

buffer(Content) ->
  receive
    stop ->
      true;
    {get, From} ->
      if
        Content =:= [] ->
          From ! empty,
          buffer([]);
        true ->
          [H|T] = Content,
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
    true -> Father ! {self(), done}
  end.

consumer(Buffer) ->
  Buffer ! {get, self()},
  receive
    stop ->
      true;
    empty ->
      io:format("~w: empty buffer~n", [self()]),
      consumer(Buffer);
    V ->
      io:format("~w consumed ~p~n", [self(), V]),
      consumer(Buffer)
  end.

main() ->
  B = spawn(?MODULE, buffer, [[]]),
  P1 = spawn(?MODULE, producer, [0,10,B,self()]),
  P2 = spawn(?MODULE, producer, [11, 20,B,self()]),
  C1 = spawn(?MODULE, consumer, [B]),
  C2 = spawn(?MODULE, consumer, [B]),
  receive
    {P1, done} ->
      true
  end,
  receive
    {P2, done} ->
      true
  end,
  C1 ! stop,
  C2 ! stop,
  B ! stop.
    
