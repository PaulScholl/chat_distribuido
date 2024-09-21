-module(user_registry).
-behaviour(gen_server).

%% API
-export([start_link/0, register_user/1, get_users/0, send_message/2]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {users = []}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_user(User) ->
    gen_server:call(?MODULE, {register, User}).

get_users() ->
    gen_server:call(?MODULE, get_users).

send_message(From, Msg) ->
    gen_server:cast(?MODULE, {send_message, From, Msg}).

%% gen_server Callbacks
init([]) ->
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    case Request of
        {register, User} ->
            io:format("User ~p connected.~n", [User]),
            {reply, ok, State#state{users = [User | State#state.users]}};
        get_users ->
            {reply, State#state.users, State}
    end.

handle_cast({send_message, From, Msg}, State) ->
    lists:foreach(fun(User) -> User ! {message, From, Msg} end, State#state.users),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
