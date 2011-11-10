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
