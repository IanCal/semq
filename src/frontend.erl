% Copyright 2011 Ian Calvert
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

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
      {ok, Message};
    {error, Reason} ->
      {error, Reason}
    after 30000 ->
      {error, timeout}
  end.
