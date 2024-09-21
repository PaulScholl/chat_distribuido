-module(chat_user).
-behaviour(gen_server).

%% API
-export([start_link/1, send_message/2]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {username}).

%% API Functions
start_link(Username) ->
    gen_server:start_link({local, Username}, ?MODULE, Username, []).

send_message(Username, Msg) ->
    gen_server:cast(Username, {send_message, Msg}).

%% gen_server Callbacks
init(Username) ->
    user_registry:register_user(self()),
    {ok, #state{username = Username}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send_message, Msg}, State) ->
    user_registry:send_message(State#state.username, Msg),
    {noreply, State}.

handle_info({message, From, Msg}, State) ->
    io:format("[~p] ~p: ~p~n", [State#state.username, From, Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
