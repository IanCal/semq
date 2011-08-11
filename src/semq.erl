%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc semq.

-module(semq).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).
-import(routing).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the semq server.
start() ->
    semq_deps:ensure(),
    ensure_started(crypto),
    application:start(semq).


%% @spec stop() -> ok
%% @doc Stop the semq server.
stop() ->
    application:stop(semq).
