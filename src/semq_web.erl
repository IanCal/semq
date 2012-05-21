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

queue_get_head(Queue, Req) ->
  case frontend:getrequest(Queue) of
        {ok, {Type, Message}} ->
            {ok, _Transport, Socket} = cowboy_http_req:transport(Req),
            case gen_tcp:recv(Socket, 0, 0) of 
              {error, 'timeout'} ->
                frontend:removehead(Queue),
                {ok, {Type, Message}, Req};
               _ ->
                 error_logger:error_report(["client disconnected"])
             end;
        {error, Reason} ->
            {error, Reason}
    end.


queue_post(Queue, Req) ->
  {ok, Body, _Req} = cowboy_http_req:body(Req),
  {Type, Req2} = cowboy_http_req:header('Content-Type', Req),
  post_message(Queue, Body, Type, Req2).

post_message(Queue, Body, undefined, Req) ->
  post_message(Queue, Body, <<"text/plain">>, Req);

post_message(Queue, Body, Type, Req) ->
  frontend:postrequest(Queue, {Type, Body}),
  {ok, {<<"text/plain">>, <<"Posted">>}, Req}.


jsonp_wrapper(undefined, MimeType, Body) ->
  {MimeType, Body};

jsonp_wrapper(Callback, _MimeType, Body) ->
  Result = <<Callback/binary, "(", Body/binary, ");">>,
  {<<"application/javascript">>, Result}.

reply(Status, MimeType, Body, Req) ->
  {Callback, Req2} = cowboy_http_req:qs_val(<<"jsonp">>, Req),
  {MimeType2, WrappedBody} = jsonp_wrapper(Callback, MimeType, Body),
  cowboy_http_req:reply(Status, headerswithtype(MimeType2), WrappedBody, Req2).

headerswithtype(Mimetype) ->
 [{<<"Access-Control-Allow-Headers">>, <<"Content-Type">>}, {<<"Access-Control-Allow-Origin">>, <<"*">>}, {<<"Content-Type">>, Mimetype}].
  

handle_method('GET', Req) ->
  {Path, Req} = cowboy_http_req:path(Req),
  {ok, Queue} = extract_queue_name(Path),
  {ok, {Type, Message}, Req2} = queue_get_head(Queue, Req),
  reply(200, Type, Message, Req2);
  

handle_method('POST', Req) ->
  {Path, Req} = cowboy_http_req:path(Req),
  {ok, Queue} = extract_queue_name(Path),
  {ok, {Type, Message}, Req2} = queue_post(Queue, Req),
	cowboy_http_req:reply(200, headerswithtype(Type), Message, Req2).

handle(Req, State) ->
  {Method, Req} = cowboy_http_req:method(Req),
  {ok, Req2} = handle_method(Method, Req),
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
