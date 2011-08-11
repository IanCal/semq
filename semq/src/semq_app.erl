%% @author Mochi Media <dev@mochimedia.com>
%% @copyright semq Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the semq application.

-module(semq_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for semq.
start(_Type, _StartArgs) ->
    semq_deps:ensure(),
    semq_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for semq.
stop(_State) ->
    ok.
