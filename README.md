# chat_distribuido
Proyecto de chat distribuido en erlang
# Programación realizada en chat_client.erl
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
# Progamación realizada en chat_server.erl

#**Tutorial**
1. Compilar ambos modulos con el siguiente comando: erl -make
2. Abrir una terminal para crear el nodo servidor con el siguiente comando:
   **erl -sname server -setcookie chat**.
3. En esta terminal abrimos el servidor con el siguiente comando:
   **chat_server:start_link().**
   Con eso ya estaremos listo para recibir usarios.
4. Para las terminales de los clientes usaremos el siguinete comando:
   **erl -sname client1 -setcookie chat**.
   El numero del cliente es meramente una referencia para llevar un conteo, debemos especificar la cookie chat para establecer una correcta conexion.
5. Una vez en la Shell de Erlang desde un nodo cliente entablaremos la conexion con el nodo servidor con el siguiente comando:
   **net_adm:ping('server@NombreDeLaMaquina').**
6. Registraremos nuestra terminal con el siguiente comando:
   **chat_client:add_user(nombreDeUsuario).**
7. Para mandar mensajes usaremos el siguiente comando en nuestra terminal cliente:
   **chat_client:send_message(nombreDeUsuario,"Nuestro mensaje").**
8. Y con ello podremos ver nuestros mensajes en nuestra terminal servidor.
