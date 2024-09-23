-module(chat_server).
-behaviour(gen_server).

%% API
-export([start_link/0, send_message/2, get_messages/0, add_user/1, remove_user/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {users = [], messages = []}).

%% Start the server
start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    global:register_name(chat_server, Pid),
    {ok, Pid}.

%% API Functions
send_message(User, Message) ->
    gen_server:cast({global, chat_server}, {send_message, User, Message}).

get_messages() ->
    gen_server:call({global, chat_server}, get_messages).

add_user(User) ->
    gen_server:call({global, chat_server}, {add_user, User}).

remove_user(User) ->
    gen_server:call({global, chat_server}, {remove_user, User}).

%% gen_server Callbacks
init([]) ->
    {ok, #state{}}.

handle_call(get_messages, _From, State) ->
    {reply, State#state.messages, State};

handle_call({add_user, User}, _From, State) ->
    case lists:member(User, State#state.users) of
        true -> 
            {reply, {error, already_exists}, State};
        false -> 
            NewUsers = [User | State#state.users],
            {reply, ok, State#state{users = NewUsers}}
    end;

handle_call({remove_user, User}, _From, State) ->
    NewUsers = lists:delete(User, State#state.users),
    {reply, ok, State#state{users = NewUsers}}.

handle_cast({send_message, User, Message}, State) ->
    NewMessages = [{User, Message} | State#state.messages],
    io:format("~p: ~p~n", [User, Message]),
    {noreply, State#state{messages = NewMessages}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.