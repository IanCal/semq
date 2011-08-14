%% Code largely taken from http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-1

-module(routing).
-behaviour(gen_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([getqueue/1]).

-import(messagequeue).

-define(SERVER, global:whereis_name(?MODULE)).

-record(state, {id2pid, pid2id}).

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

getqueue(QueueName) ->
  gen_server:call(?SERVER, {getqueue, QueueName}).


init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{
              id2pid = ets:new(?MODULE, [bag]),
              pid2id = ets:new(?MODULE, [bag])
             }
  }.

handle_call({getqueue, QueueName}, _From, State) ->
  case ets:lookup(State#state.id2pid, QueueName) of
    [] ->
      Queue = spawn(messagequeue, new, []),
      link(Queue),
      ets:insert(State#state.id2pid, {QueueName, Queue}),
      ets:insert(State#state.pid2id, {Queue, QueueName}),
      {reply, {ok, Queue}, State};
    [{QueueName, QueuePid}] ->
      {reply, {ok, QueuePid}, State}
  end;

handle_call({close, Pid}, _From, State) when is_pid(Pid) ->
    unlink(Pid),
    [{Pid, Id}] = ets:lookup(State#state.pid2id, Pid),
    ets:delete(State#state.pid2id, Pid),
    ets:delete(State#state.id2pid, Id),
    {reply, ok, State};

handle_call(stop, _From, State) -> 
    {stop, normal, ok, State}.



handle_info(Info, State) ->
  case Info of
    {'EXIT', Pid, _Why} ->
      handle_call({close, Pid}, ok, State); 
    UnknownError ->
      io:format("Caught unhandled message: ~w\n", [UnknownError])
  end,
  {noreply, State}.

stop() ->
    gen_server:call(?SERVER, stop).



handle_cast(_Msg, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

