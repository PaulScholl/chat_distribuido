# chat_distribuido
Proyecto de chat distribuido en erlang
<br/>**Equipo:**
<br/>1.BEJARANO DUQUE ANTOINE 
<br/>2.RODRIGUEZ GALLARDO PAUL 

#**Tutorial**
1. Compilar ambos modulos con el siguiente comando: erl -make
2. Abrir una terminal para crear el nodo servidor con el siguiente comando:<br/>**erl -sname server -setcookie chat**.
3. En esta terminal abrimos el servidor con el siguiente comando:<br/>**chat_server:start_link().**
   Con eso ya estaremos listo para recibir usarios.
4. Para las terminales de los clientes usaremos el siguinete comando:<br/>**erl -sname client1 -setcookie chat**.
   <br/>El numero del cliente es meramente una referencia para llevar un conteo, debemos especificar la cookie chat para establecer una correcta conexion.
5. Una vez en la Shell de Erlang desde un nodo cliente entablaremos la conexion con el nodo servidor con el siguiente comando:<br/>**net_adm:ping('server@NombreDeLaMaquina').**
6. Registraremos nuestra terminal con el siguiente comando:<br/>**chat_client:add_user(nombreDeUsuario).**
7. Para mandar mensajes usaremos el siguiente comando en nuestra terminal cliente:<br/>**chat_client:send_message(nombreDeUsuario,"Nuestro mensaje").**
8. Y con ello podremos ver nuestros mensajes en nuestra terminal servidor.

**Todos los puntos de vista de la ejecución**
https://asciinema.org/a/tdg62GKY2E61mOjY2hSyUT2Aw <br/> https://asciinema.org/a/FFV6crOGj5OfTki1g8O90qsno <br/> https://asciinema.org/a/NjhYyqAkRC80ueeokMTcJbWAU
