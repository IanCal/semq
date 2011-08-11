-module(frontend).
-import(routing).
-export([getrequest/1, postrequest/2]).

postrequest(QueueName, Message) ->
  {ok, Pid} = routing:getqueue(QueueName),
  Pid ! {add, Message}.

getrequest(QueueName) ->
  {ok, Pid} = routing:getqueue(QueueName),
  request(Pid).

request(Pid) ->
  Pid ! {get, self()},
  receive
    {ok, Message} ->
      Pid ! message_received,
      {ok, Message}
    after 30000 ->
      {error, timeout}
  end.
