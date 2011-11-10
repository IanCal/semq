SEMQ

SEMQ is a simple message queue system built in erlang. The purpose is to allow fast push-based text communication between systems capable of basic (GET and POST) HTTP requests.

You are able to do three things.


1) Put a message on a named queue. If the queue does not exist, it will be created for you.

2) Get the next message in a queue (this will delete the message). If no queue exists it will be created.

3) Get a list of all currently active queues (note : there may be no client connected to a queue)

If you try and get a message from an empty queue, then the server will wait until either 30s have passed or a message is pushed onto the queue. This enables a simple client to get messages pushed to it instead of using quick polling.

After 5 minutes of inactivity on a queue, it will get destroyed and any messages on it will be cleared. 

INSTALLATION

First you need erlang

Linux:

You'll need the dev package for erlang, built with ssl, and the inets package. On ubuntu:

    apt-get install erlang-dev erlang-inets

Mac (only checked on Snow Leopard and Lion)

I suggest using macports, then:

    port install erlang +ssl


Now you can build semq

     make


RUNNING

Simply start the dev server:

    ./start-dev.sh

The server will be running on port 8080


USE

There is a pre-built ruby gem for using this server at https://rubygems.org/gems/semq-client (code at https://github.com/IanCal/ruby-semq)

Use as follows in two terminals:

Terminal 1:

    irb -r rubygems
    >> require 'semq_client'
    >> queue = SemqClient.new("http://localhost:8080", "queuename")
    >> queue.pop

Terminal 2:

    irb -r rubygems
    >> require 'semq_client'
    >> queue = SemqClient.new("http://localhost:8080", "queuename")
    >> queue.push("Hello World!")

You should see "Hello World!" appear in terminal 1

Other Implementations

Pseudo code for a client waiting for information:

    while true:
      message = http.get("http://messageserver/queue/myqueuename")
      if (message):
        onMessageCallback(message)

Or an asynchronous version:

    function makeRequest() :
      httpRequest = http.get("http://messageserver/queue/myqueuename") 
      httpRequest.onSuccess = onSuccess
      httpRequest.onError = onError
    
    function onSuccess(result) :
      callback(result)
      makeRequest()
    
    function onError(result) :
      makeRequest()
