-module(test).
-compile(export_all).


server(Port) ->
  {ok, ListenSocket} = gen_tcp:listen(Port, [{mode, list},{active, false}]),
  wait_connect(ListenSocket,0).

wait_connect(ListenSocket, Count) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(?MODULE, wait_connect, [ListenSocket, Count+1]),
  dispatcher(Socket).
  
dispatcher(Socket) ->
  {ok,CMD} = gen_tcp:recv(Socket, 0),
  case {ok, string:tokens(CMD, " ")} of
  {ok, ["CON", Usuario]} ->
    io:format(Usuario),
    spawn(?MODULE, psocket, [Socket, Usuario]); 
  True ->
    io:format("La batiste")
end.
   
  
psocket(Socket, Name) ->
  
  io:format("Llegue a un psocket ~n"),

 {ok, CMD} = gen_tcp:recv(Socket, 0),
 case {ok, string:tokens(CMD, " ")} of
   {ok, "LSG\r\n"} ->
      io:format("LSG+");
   {ok, "NEW\r\n"} ->
      io:format("NEW+");
   {ok, "ACC\r\n"} ->
      io:format("ACC+");
   {ok, "PLA\r\n"} ->
      io:format("PLA+");
   {ok, "OBS\r\n"} ->
      io:format("OBS+");
   {ok, "LEA\r\n"} ->
      io:format("LEA+");
   {ok, "BYE\r\n"} ->
      io:format("BYE+");
   {ok, Cosa} ->
      io:format("~p~n",[Cosa])
 end.
