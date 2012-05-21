%%%----------------------------------------------------------------
%%% @author  Ian Calvert <ianjcalvert@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2012 Ian Calvert
%%%----------------------------------------------------------------
-module(semq_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

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
          {[<<"queues">>], semq_queuelist, []},
          {['...'], semq_web, []}
          ]}
    ],
      
    cowboy:start_listener(web_frontend, 100,
      cowboy_tcp_transport, [{port, 8080}],
      cowboy_http_protocol, [{dispatch, Dispatch}]
    ),
   semq_sup:start_link().

%% @private
-spec stop(State::any()) -> ok.
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
