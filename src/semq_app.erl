%%%----------------------------------------------------------------
%%% @author  Ian Calvert <ianjcalvert@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2012 Ian Calvert
%%%----------------------------------------------------------------
-module(semq_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, get_port/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================


%% @private
-spec start(normal | {takeover, node()} | {failover, node()},
            any()) -> {ok, pid()} | {ok, pid(), State::any()} |
                      {error, Reason::any()}.
start(_StartType, _StartArgs) ->
    Dispatch = [
        %% {Host, list({Path, Handler, Opts})}
        {'_', [
          {[<<"favicon.ico">>], semq_favicon, []},
          {[<<"crossdomain.xml">>], semq_crossdomain, []},
          {[<<"queues">>], semq_queuelist, []},
          {[<<"index.html">>], semq_indexpage, []},
          {[], semq_indexpage, []},
          {['...'], semq_web, []}
          ]}
    ],
      
    cowboy:start_listener(web_frontend, 100,
      cowboy_tcp_transport, [{port, get_port()}],
      cowboy_http_protocol, [{dispatch, Dispatch}]
    ),
   semq_sup:start_link().

get_port() ->
   parse_port(init:get_argument(port)).

parse_port(error) ->
   8080;
parse_port({ok,[[Port]]}) ->
   {PortNumber, []} = string:to_integer(Port),
   PortNumber.

%% @private
-spec stop(State::any()) -> ok.
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
