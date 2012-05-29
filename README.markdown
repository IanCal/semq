#SEMQ#

SEMQ is a simple message queue system built in erlang. The purpose is to allow fast communication between systems capable of basic (GET and POST) HTTP requests.

##API##

For a single queue

    HTTP POST    to http://semqserver:port/queue/my_awesome_queue_name
          - put the body and mimetype on the queue
    HTTP POST    to http://semqserver:port/queue/my_awesome_queue_name/queue/my_other_awesome_queue_name
          - put the body and mimetype on both queues
    
    HTTP GET     to http://semqserver:port/queue/my_awesome_queue_name 
          - get the next item on the queue, served with the same mimetype as used in the POST
          - if there is nothing in the queue, wait for 30 seconds then return a 404
    HTTP GET     to http://semqserver:port/queue/my_awesome_queue_name?jsonp=callbackName 
          - get the next item on the queue, served with application/javascript, containing callbackName(messageBody);
          - if there is nothing in the queue, wait for 30 seconds then return a 200 containing callbackName();
    
    HTTP DELETE  to http://semqserver:port/queue/my_awesome_queue_name
          - will delete the queue and all messages on it
    
Other endpoints
    
    HTTP GET     to http://semqserver:port/queues
          - get a list of all queues currently running on the server
    
    HTTP GET     to http://semqserver:port/
          - show a welcome screen
    
    HTTP GET     to http://semqserver:port/crossdomain.xml
          - return a crossdomain file for actionscript, allowing connections forom any server
    
##NOTES##

To create a queue, simply use it. If it doesn't exist it'll be created for you.

After 5 minutes of inactivity on a queue, it will get destroyed and any messages on it will be cleared. 

There is no chunking, so the maximum size of an item is 1MB.

You are not restricted to using text, all data is considered binary so you can easily drop images or whatever you want on the queues. Just make sure the other side knows what to expect!

Only one client should *listen* to a queue at the same time. Many people can post to a queue at the same time.

##INSTALLATION##

For mac and linux, there are binaries available on the downloads page.

##COMPILATION##

I use sinan to compile the application. Install erlang, cowboy (tested on 0.4.0 -> 0.6.0) and gproc, and run

    > sinan release

You'll find semq in _build/semq/bin

##RUNNING##

Unzip to wherever you want and run 

    > bin/semq -detached -port 8080 

Then check it's running at http://localhost:8080/

##USE##

####Ruby####

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

####Other Implementations####

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
