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


-module(semq_web).
-behaviour(cowboy_http_handler).

-import(frontend).
-import(gproc).


-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.



extract_queue_name([<<"queue">> | [Queue | _]]) ->
  {ok, Queue};
  
extract_queue_name([_Key | [_Value | Remainder]]) ->
  extract_queue_name(Remainder);

extract_queue_name(_) ->
  {error, "No queue name in URL"}.

queue_access('GET', Queue, _Req) ->
  frontend:getrequest(Queue);

queue_access('POST', Queue, Req) ->
  {ok, Body, _Req} = cowboy_http_req:body(Req),
  frontend:postrequest(Queue, Body),
  {ok, <<"Posted">>}.

handle(Req, State) ->
  {Path, Req} = cowboy_http_req:path(Req),
  {ok, Queue} = extract_queue_name(Path),
  {Method, Req} = cowboy_http_req:method(Req),
  {ok, Message} = queue_access(Method, Queue, Req),
	{ok, Req2} = cowboy_http_req:reply(200, [], Message, Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.



%% External API

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

fake_get_request(Queue) ->
    Result = frontend:getrequest(Queue),
    io_lib:format("WHAAAAT~n",[]),
    frontend:removehead(Queue),
    Result.

pushing_onto_queue_after_get_still_retrieves_message_test() ->
    gproc:start_link(),
    Message = "a message to send", 
    Queue = "queueNameNew",
    Pid = spawn(fun() -> fake_get_request(Queue) end),
    timer:sleep(100),
    frontend:postrequest(Queue, "some kind of message"),
    frontend:postrequest(Queue, Message),
    timer:sleep(100),
    RecievedMessage = fake_get_request(Queue),
    ?assertEqual(
       {ok, Message},
       RecievedMessage),
    ok.

messages_put_onto_queue_can_be_retrieved_test() ->
    gproc:start_link(),
    Message = "a message to send", 
    frontend:postrequest("queueName", Message),
    RecievedMessage = fake_get_request("queueName"),
    ?assertEqual(
       {ok, Message},
       RecievedMessage),
    ok.

messages_put_onto_queue_can_be_retrieved_in_correct_order_test() ->
    gproc:start_link(),
    Queue = "queueName2",
    Messages = ["one", "two", "three", "four"], 
    lists:foreach(fun(M) -> frontend:postrequest(Queue, M) end, Messages),
    RecievedMessages = lists:map(fun (_) -> fake_get_request(Queue) end, Messages),
    ExpectedMessages = lists:map(fun (M) -> {ok, M} end, Messages),
    ?assertEqual(
       ExpectedMessages,
       RecievedMessages),
    ok.

if_request_dies_message_remains_in_queue_test() ->
    gproc:start_link(),
    Message = "a message to send", 
    Queue = "queueName",
    Pid = spawn(frontend, getrequest, [Queue]),
    exit(Pid, "reason"),
    frontend:postrequest(Queue, Message),
    RecievedMessage = fake_get_request(Queue),
    ?assertEqual(
       {ok, Message},
       RecievedMessage),
    ok.



-endif.
