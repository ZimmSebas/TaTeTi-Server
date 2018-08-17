-module(test).
-compile(export_all).


server(Port) ->
  {Damian, ListenSocket} = gen_tcp:listen(Port, [{active, false}]),
  io:format("Server lanzado escuchando puerto: ~p", [Port]),
  io:format(" en cosito ~p~n",[Damian]),
  io:format("en Socket ~p~n",[ListenSocket]),
  
  %spawn(?MODULE,dispatcher, [ListenSocket,Port]).
  
  dispatcher(ListenSocket,Port).
  
dispatcher(ListenSocket, Port) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  % spawn(?MODULE, dispatcher, [Port]),
  psocket1(Socket),
  server(Port).
  
psocket1(Socket) ->
  {ok,CMD} = gen_tcp:recv(Socket, 0),
  case {ok, string:tokens(CMD, " ")} of
  {ok, ["CON", Usuario]} ->
    io:format(Usuario),
    spawn(?MODULE, psocket2, [Socket, Usuario]) 
 end.
   
  
psocket2(Socket, Name) ->
  
  io:format("Llegue a un psocket ~n"),
  io:format("en Socket p2 ~p~n",[Socket]),
    
  
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
      io:format("Error ud mando ~p envíe un comando descente, infeliz ~n",[Cosa])
end,
  psocket2(Socket,Name).