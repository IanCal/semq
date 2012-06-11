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


-module(semq_indexpage).
-behaviour(cowboy_http_handler).

-import(frontend).
-import(gproc).


-export([init/3, handle/2, terminate/2, format_queue/1, format_queues/1]).

init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.

format_queue(Name) ->
  [Name, <<"<br>">>].

format_queues({ok, []}) ->
  <<"There are no queues currently running">>;

format_queues({ok, Queues}) ->
  [<<"The following queues are currently running:<br>">>, lists:map(fun semq_indexpage:format_queue/1, Queues)].

get_version() ->
  {ok, Vsn} = application:get_key(semq, vsn),
  list_to_binary(Vsn).
  

handle(Req, State) ->
  {ok, Req2} = cowboy_http_req:reply(200, headerswithtype("text/html"), [<<"
  <html>
  <head>
  </head>
  <body>
  Hooray! SEMQ version ">>, get_version() ,<<" is running correctly!<br>">>,
  format_queues(frontend:get_all_queue_names()),
  <<"
  </body>
  </html>
">>], Req),
	{ok, Req2, State}.

headerswithtype(Mimetype) ->
 [{<<"Content-Type">>, Mimetype}].

terminate(_Req, _State) ->
	ok.


