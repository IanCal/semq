-module(messagequeue).
-export([new/0, getfrom/1]).

new() ->
  queue([]).

queue([]) ->
  receive 
    {get, PidReturn} ->
      returnnextadd(PidReturn);
    {add, Message} ->
      queue([Message])
  end;

queue(Messages) ->
  receive 
    {get, PidReturn} ->
      [Next | Remaining] = Messages,
      PidReturn ! {ok, Next},
      queue(Remaining);
    {add, Message} ->
      queue(Messages ++ [Message])
  end.

% Error condition if Pid is not available?
returnnextadd(Pid) ->
  receive
    {add, Message} ->
      Pid ! {ok, Message},
      queue([]);
    {get, PidReturn} ->
      returnnextadd(PidReturn)
  end.
