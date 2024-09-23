-module(chat_client).

%% API
-export([add_user/1, send_message/2, get_messages/0, remove_user/1]).

%% Funciones para interactuar con el servidor
add_user(User) ->
    gen_server:call({global, chat_server}, {add_user, User}).

remove_user(User) ->
    gen_server:call({global, chat_server}, {remove_user, User}).

send_message(User, Message) ->
    gen_server:cast({global, chat_server}, {send_message, User, Message}).

get_messages() ->
    gen_server:call({global, chat_server}, get_messages).