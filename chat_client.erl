-module(chat_client).  %% Define el módulo llamado chat_client

%% API
-export([add_user/1, send_message/2, get_messages/0, remove_user/1]).
%% Exporta las funciones públicas del módulo para que puedan ser llamadas externamente.
%% add_user/1: agrega un usuario al servidor.
%% send_message/2: envía un mensaje al servidor.
%% get_messages/0: obtiene los mensajes almacenados en el servidor.
%% remove_user/1: elimina un usuario del servidor.

%% Funciones para interactuar con el servidor

%% add_user(User)
%% Llama al servidor de chat global (chat_server) para agregar un usuario.
add_user(User) ->
    gen_server:call({global, chat_server}, {add_user, User}).

%% remove_user(User)
%% Llama al servidor de chat global (chat_server) para eliminar un usuario.
remove_user(User) ->
    gen_server:call({global, chat_server}, {remove_user, User}).

%% send_message(User, Message)
%% Envía un mensaje desde un usuario específico al servidor de chat de forma asíncrona.
send_message(User, Message) ->
    gen_server:cast({global, chat_server}, {send_message, User, Message}).

%% get_messages()
%% Recupera todos los mensajes almacenados en el servidor de chat.
get_messages() ->
    gen_server:call({global, chat_server}, get_messages).
