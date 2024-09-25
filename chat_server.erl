-module(chat_server).  %% Define el módulo llamado chat_server
-behaviour(gen_server).  %% Especifica que este módulo sigue el comportamiento de un gen_server

%% API
-export([start_link/0, send_message/2, get_messages/0, add_user/1, remove_user/1]).
%% Exporta las funciones API que se pueden utilizar para interactuar con el servidor:
%% start_link/0: inicia el servidor.
%% send_message/2: envía un mensaje al servidor.
%% get_messages/0: obtiene todos los mensajes almacenados.
%% add_user/1: agrega un usuario a la lista de usuarios.
%% remove_user/1: elimina un usuario de la lista de usuarios.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Exporta las funciones de callback que el gen_server utiliza para manejar diferentes eventos:
%% init/1: inicializa el estado del servidor.
%% handle_call/3: maneja solicitudes síncronas (gen_server:call).
%% handle_cast/2: maneja solicitudes asíncronas (gen_server:cast).
%% handle_info/2: maneja mensajes no solicitados (información externa).
%% terminate/2: se llama cuando el servidor se termina.
%% code_change/3: maneja cambios de código (actualizaciones de versiones).

-record(state, {users = [], messages = []}).
%% Define el estado del servidor con un registro que contiene:
%% users: lista de usuarios conectados.
%% messages: lista de mensajes enviados en el chat.

%% Inicia el servidor de chat y lo registra globalmente
start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),  %% Inicia el gen_server con estado inicial vacío
    global:register_name(chat_server, Pid),              %% Registra el servidor globalmente bajo el nombre chat_server
    {ok, Pid}.                                           %% Retorna el identificador del proceso (PID)

%% API Functions

%% Envía un mensaje al servidor de forma asíncrona
send_message(User, Message) ->
    gen_server:cast({global, chat_server}, {send_message, User, Message}).

%% Recupera todos los mensajes almacenados en el servidor
get_messages() ->
    gen_server:call({global, chat_server}, get_messages).

%% Agrega un usuario a la lista de usuarios del servidor
add_user(User) ->
    gen_server:call({global, chat_server}, {add_user, User}).

%% Elimina un usuario de la lista de usuarios del servidor
remove_user(User) ->
    gen_server:call({global, chat_server}, {remove_user, User}).

%% gen_server Callbacks

%% Inicializa el estado del servidor con listas vacías de usuarios y mensajes
init([]) ->
    {ok, #state{}}.

%% Maneja una llamada síncrona para obtener todos los mensajes
handle_call(get_messages, _From, State) ->
    {reply, State#state.messages, State}.  %% Responde con la lista de mensajes del estado actual

%% Maneja una llamada síncrona para agregar un usuario
handle_call({add_user, User}, _From, State) ->
    case lists:member(User, State#state.users) of
        true ->  %% Si el usuario ya existe, devuelve un error
            {reply, {error, already_exists}, State};
        false ->  %% Si no existe, agrega el usuario a la lista y actualiza el estado
            NewUsers = [User | State#state.users],
            {reply, ok, State#state{users = NewUsers}}
    end.

%% Maneja una llamada síncrona para eliminar un usuario
handle_call({remove_user, User}, _From, State) ->
    NewUsers = lists:delete(User, State#state.users),  %% Elimina el usuario de la lista
    {reply, ok, State#state{users = NewUsers}}.        %% Responde con ok y actualiza el estado

%% Maneja una solicitud asíncrona para enviar un mensaje
handle_cast({send_message, User, Message}, State) ->
    NewMessages = [{User, Message} | State#state.messages],  %% Agrega el mensaje a la lista de mensajes
    io:format("~p: ~p~n", [User, Message]),  %% Imprime el mensaje en la consola del servidor
    {noreply, State#state{messages = NewMessages}}.  %% No responde inmediatamente, pero actualiza el estado

%% Maneja cualquier otro mensaje inesperado que llegue al servidor
handle_info(_Info, State) ->
    {noreply, State}.

%% Se llama cuando el servidor se cierra, liberando recursos si es necesario
terminate(_Reason, _State) ->
    ok.

%% Maneja cambios de código en tiempo de ejecución (por ejemplo, actualizaciones de versiones)
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
