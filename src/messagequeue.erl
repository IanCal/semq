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
      queue([])
    after ?TIMEOUT ->
      exit(self())
  end;

queue(Messages) ->
  receive 
    {get, PidReturn} ->
      [Next | _] = Messages,
      PidReturn ! {ok, Next},
      queue(Messages);
    message_received ->
      [_ | Remaining] = Messages,
      queue(Remaining);
    {add, Message} ->
      queue(Messages ++ [Message])
    after ?TIMEOUT ->
      exit(self())
  end.

returnnextadd(Pid) ->
  receive
    {add, Message} ->
      Pid ! {ok, Message},
      queue([Message]);
    message_received ->
      queue([]);
    {get, PidReturn} ->
      returnnextadd(PidReturn)
    after ?TIMEOUT ->
      exit(self())
  end.