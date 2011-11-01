-module(frontend).
-import(routing).
-export([getrequest/1, postrequest/2, deleterequest/1]).

postrequest(QueueName, Message) ->
  {ok, Pid} = routing:getqueue(QueueName),
  Pid ! {add, Message}.

getrequest(QueueName) ->
  {ok, Pid} = routing:getqueue(QueueName),
  request(Pid).

deleterequest(QueueName) ->
  routing:deletequeue(QueueName).

request(Pid) ->
  Pid ! {get, self()},
  receive
    {ok, Message} ->
      Pid ! message_received,
      {ok, Message}
    after 30000 ->
      {error, timeout}
  end.
