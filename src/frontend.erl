-module(frontend).
-import(routing).
-export([getrequest/1, postrequest/2, deleterequest/1, get_all_queue_names/0, removehead/1]).

get_all_queue_names() ->
  routing:getqueues().

postrequest(QueueName, Message) ->
  {ok, Pid} = routing:getqueue(QueueName),
  Pid ! {add, Message}.

getrequest(QueueName) ->
  {ok, Pid} = routing:getqueue(QueueName),
  request(Pid).

deleterequest(QueueName) ->
  routing:deletequeue(QueueName).

removehead(QueueName) ->
  {ok, Pid} = routing:getqueue(QueueName),
  Pid ! message_received.

request(Pid) ->
  Pid ! {get, self()},
  receive
    {ok, Message} ->
      {ok, Message}
    after 30000 ->
      {error, timeout}
  end.
