-module(tp).
-compile(export_all).


server(Port) ->
  {Damian, ListenSocket} = gen_tcp:listen(Port, [{active, false}]),
  io:format("Server lanzado escuchando puerto: ~p", [Port]),
  io:format(" en cosito ~p~n",[Damian]),
  Pidver = spawn(?MODULE,verificarusuario,[[]]),
  dispatcher(ListenSocket,Pidver).
  
dispatcher(ListenSocket,Pidver) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(?MODULE, dispatcher, [ListenSocket,Pidver]),
  psocket1(Socket,Pidver).

  
psocket1(Socket,Pidver) ->
  {ok,CMD} = gen_tcp:recv(Socket, 0),
  case {ok, string:tokens(CMD, " ")} of
    {ok, ["CON", Usuario]} ->
      Pidver!{self(),veri,Usuario},
      io:format("Mande usuario ~n"),
      receive Rta ->
      io:format("~p~n",[Rta]),
        if
          Rta =:= "ok" ->
            psocket2(Socket, Usuario),      
            io:format("Psocket2 ~n"); 
          true ->      
            io:format("Usuario existente ~n"),
            psocket1(Socket,Pidver) 
        end
      end;
    {Otra, Cosa} ->
      io:format("Primero debe elegir un usuario~n")
  end.
   
verificarusuario(Lista) ->
   receive 
      {Pid,veri,Usuario} -> 
        Pid!"ok",
        io:format("Me llego un Usuario ~p~n",[Usuario]),
        verificarusuario([Usuario|Lista]);
      true -> io:format("SEROMPIOTODOMALDITASEA"),
        verificarusuario(Lista)
   end.
   
  
psocket2(Socket, Name) ->
  
 {ok, CMD} = gen_tcp:recv(Socket, 0),
 case {ok, string:tokens(CMD, " ")} of
   {ok, "LSG\r\n"} ->
      io:format("LSG+ ~p~n",Name);
   {ok, "NEW\r\n"} ->
      io:format("NEW+ ~p~n",Name);
   {ok, "ACC\r\n"} ->
      io:format("ACC+ ~p~n",Name);
   {ok, "PLA\r\n"} ->
      io:format("PLA+ ~p~n",Name);
   {ok, "OBS\r\n"} ->
      io:format("OBS+ ~p~n",Name);
   {ok, "LEA\r\n"} ->
      io:format("LEA+ ~p~n",Name);
   {ok, "BYE\r\n"} ->
      io:format("BYE+ ~p~n",Name);
   {ok, Cosa} ->
      io:format("Error ud mando ~p envie un comando descente, infeliz ~n",[Cosa])
end,
  psocket2(Socket,Name).