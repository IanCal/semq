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


-module(semq_queuelist).
-behaviour(cowboy_http_handler).

-import(frontend).
-import(gproc).


-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.



handle(Req, State) ->
  {ok, Queues} =  frontend:get_all_queue_names(),
  {ok, Req2} = cowboy_http_req:reply(200, [], io_lib:format(<<"~p~n">>, [Queues]), Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.


