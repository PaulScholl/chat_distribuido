# chat_distribuido
Proyecto de chat distribuido en erlang

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
