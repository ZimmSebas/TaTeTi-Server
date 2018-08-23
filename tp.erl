-module(tp).
-compile(export_all).


server(Port, Nodo) ->
  if 
    Nodo /= main ->net_kernel:connect_node(main);
    true -> global:register_name(main, self())
  end,
  %io:format("~p",global:registered_names()),
  spawn(?MODULE, prueba, []),
  {Damian, ListenSocket} = gen_tcp:listen(Port, [{active, false}]),
  io:format("Server lanzado escuchando puerto: ~p", [Port]),
  io:format(" en cosito ~p~n",[Damian]),
  Pidver = spawn(?MODULE,verificarusuario,[[],Port]),
  dispatcher(ListenSocket,Pidver).
  
prueba() ->
  receive
    {ok, prueba} ->
      io:format("FUNCIONO")
  end.
  
dispatcher(ListenSocket,Pidver) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(?MODULE, dispatcher, [ListenSocket,Pidver]),
  psocket1(Socket,Pidver).

  
psocket1(Socket,Pidver) ->
  {ok,CMD} = gen_tcp:recv(Socket, 0),
  case {ok, string:tokens(CMD, " ")} of
    {ok, ["CON", Usuario]} ->
      global:send(main, "prueba"),
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
    {_, _} ->
      io:format("Primero debe elegir un usuario~n"),
      psocket1(Socket,Pidver)
  end.
   
verificarusuario(Lista,Port) ->
 receive 
  {Pid,veri,Usuario} -> 
    case lists:member(Usuario, Lista) of
      false ->
	io:format("Me llego un Usuario ok ~p~n",[Usuario]),
	Pid!"ok",
	verificarusuario([Usuario|Lista],Port);
      true ->
	io:format("Me llego un Usuario notok ~p~n",[Usuario]),
	verificarusuario(Lista,Port),
	Pid!"notok"
    end
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
  {ok, "OBS\r\n"} ->
    io:format("OBS+ ~p~n",Name);
  {ok, "BYE\r\n"} ->
    io:format("BYE+ ~p~n",Name);
  {ok, _} ->
io:format("Error ud mando un comando indescente, infeliz ~n")
end,
  psocket2(Socket,Name).



% {ok, "PLA\r\n"} ->
%   io:format("PLA+ ~p~n",Name);

%   {ok, "LEA\r\n"} ->
%      io:format("LEA+ ~p~n",Name);
