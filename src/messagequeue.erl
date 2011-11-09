-module(messagequeue).
-export([new/0]).
-define(TIMEOUT, 300000). 

new() ->
  queue([]).

queue([]) ->
  receive 
    {get, PidReturn} ->
      returnnextadd(PidReturn);
    {add, Message} ->
      queue([Message]);
    message_received ->
      queue([]);
    empty_queue ->
      exit(self())
    after ?TIMEOUT ->
      exit(self())
  end;

queue(Messages) ->
  receive 
    {get, PidReturn} ->
      [Next | _] = Messages,
      PidReturn ! {ok, Next},
      queue(Messages);
    {add, Message} ->
      queue(Messages ++ [Message]);
    message_received ->
      [_ | Remaining] = Messages,
      queue(Remaining);
    empty_queue ->
      exit(self())
    after ?TIMEOUT ->
      exit(self())
  end.

returnnextadd(Pid) ->
  receive
    {get, PidReturn} ->
      returnnextadd(PidReturn);
    {add, Message} ->
      Pid ! {ok, Message},
      queue([Message]);
    message_received ->
      queue([]);
    empty_queue ->
      exit(self())
    after ?TIMEOUT ->
      exit(self())
  end.
