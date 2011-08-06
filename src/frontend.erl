-module(frontend).
-import(routing).
-export([getrequest/1, postrequest/2]).

postrequest(QueueName, Message) ->
  {ok, Pid} = routing:getqueue(QueueName),
  Pid ! {add, Message}.

getrequest(QueueName) ->
  {ok, Pid} = routing:getqueue(QueueName),
  Response = request(Pid),
  io:format("would now return to webside the response ~p~n", [Response]).

request(Pid) ->
  Pid ! {get, self()},
  receive
    {ok, Message} ->
      Pid ! message_received,
      Message
    after 30000 ->
      io:format("timeout error~n"),
      {error, timeout}
  end.
