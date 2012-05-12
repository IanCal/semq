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
-import(messagequeue).
-export([getrequest/1, postrequest/2, deleterequest/1, get_all_queue_names/0, removehead/1, get_queue/1]).


get_queue(Name) ->
   create_if_not_exist(gproc:lookup_local_name(Name), Name).

create_if_not_exist(undefined, Name) ->
   spawn(fun() ->
                    gproc:add_local_name(Name),
                    messagequeue:new()
                end),
   {Pid, _} = gproc:await({n,l,Name}),
   {ok, Pid};
create_if_not_exist(Pid, _Name) ->
    {ok, Pid}.



get_all_queue_names() ->
  {ok, [Name || {{_Type, _Scope, Name}, _Pid, _} <- qlc:e(gproc:table(n), [unique_all])]}.

postrequest(QueueName, Message) ->
  {ok, Pid} = get_queue(QueueName),
  Pid ! {add, Message}.

getrequest(QueueName) ->
  {ok, Pid} = get_queue(QueueName),
  request(Pid).

deleterequest(QueueName) ->
  {ok, Pid} = get_queue(QueueName),
  Pid ! empty_queue.

removehead(QueueName) ->
  {ok, Pid} = get_queue(QueueName),
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
