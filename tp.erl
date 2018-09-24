% El cliente (cliente) llega a la puerta del bar (servidor) en donde es atendido por el recepcionista (dispatcher). Este lo dirige a la mesa en donde sera antendido por un mozo (psocket).
% El cliente le indica su pedido al mozo, y este se dirige a hablar con el supervisor (pbalance). El supervisor le indica al mozo cual es la sucursal con menor carga de trabajo.
% El mozo se dirige a la sucursal indicada por su supervisor y le solicita al cocinero (pcomando) el pedido del cliente. Una vez listo el pedido, el cocinero le notifica al mozo, el cual a su vez notifica al cliente.
% Notese que para que el supervisor pueda indicarle al mozo cual es la sucursal con menor carga de trabajo, es necesario que se mantenga en contacto con los telefonistas (pstat) de todas las sucursales.
% Estos se encargan de informar a los supervisores de las diferentes sucursales, cual es la carga de trabajo del local.

% erl -name name@ip
-module(tp).
-define(SERVERS, ['nodo1@127.0.0.1','nodo2@127.0.0.1']).
-define(LOADS, [999, 999]).
-define(TABLEROINICIAL, [[1,1,1],[1,1,1],[1,1,1]]).
-export([init/0, load/0, transpose/1, winner/1, checkuser/1, dispatcher/1, gamelist/1, psocket/2, pcomando/4, pstat/0, pbalance/1]).
%-compile(export_all).

load() -> length(erlang:ports()).

transpose([ [] | _ ]) -> [];
transpose(M) -> [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].
  
% Determina si que jugador gano (1/2), si hay empate (3) o nada de lo anterior (0).
winner(Tablero) ->
  Sumas = [ lists:sum(lists:nth(X, Tablero)) || X <- [1,2,3] ] ++ 
  [ lists:sum(lists:nth(X, transpose(Tablero))) || X <- [1,2,3] ] ++ 
  [ lists:nth(1, lists:nth(1, Tablero)) + lists:nth(2, lists:nth(2, Tablero)) + lists:nth(3, lists:nth(3, Tablero)) ] ++ 
  [ lists:nth(1, lists:nth(3, Tablero)) + lists:nth(2, lists:nth(2, Tablero)) + lists:nth(3, lists:nth(1, Tablero)) ],
  
  Ganador1 = lists:member(9, Sumas),
  Ganador2 = lists:member(27, Sumas),
  Empate =  lists:member(1,lists:nth(1,Tablero)) or lists:member(1,lists:nth(2,Tablero)) or lists:member(1,lists:nth(3,Tablero)),
  
  if
    Ganador1 -> 1;
    Ganador2 -> 2;
    not Empate -> 3;
    true -> 0
  end.

% Inicializa el servidor
init() ->
	{Atomo, ListenSocket} = gen_tcp:listen(0, [{active, false}]),
	if
		Atomo /= ok -> io:format(">> Se ha producido un error ");
		true ->
			{ok, Port} = inet:port(ListenSocket), % Detecta el puerto
			[net_adm:ping(Node) || Node <- ?SERVERS], % Reconoce a los otros nodos
			spawn(?MODULE, dispatcher, [ListenSocket]), % Lanza el dispatcher
			spawn(?MODULE, pstat, []), % Lanza el pstat
			register(gamelist, spawn(?MODULE, gamelist, [[]])), % Lanza el gamelist
			register(checkuser, spawn(?MODULE, checkuser, [[]])), % Lanza el registro de usuarios
			register(pbalance, spawn(?MODULE, pbalance, [lists:zip(?SERVERS, ?LOADS)])), % Lanza el pbalance
			io:format(">> Servidor ~p escuchando en puerto: ~p.~n>> Asegurese de iniciar el resto de los servidores antes de comenzar ", [node(), Port])
	end.

checkuser(UserList) ->
		receive
			% Imprime por terminal la carga de los nodos
			{print} -> io:format(">> Lista de usuarios: ~p", [UserList]), checkuser(UserList);

			% Agrega un usuario a la lista
			{add, User} -> checkuser([User | UserList]);

			% Verifica si el nombre de usuario esta en uso
			{Who, User} ->
				case lists:member(User, UserList) of
					true -> Who!{error}, checkuser(UserList); % Avisa que el nombre de usuario esta ocupado
					_ ->
						[{checkuser, Node}!{add, User} || Node <- nodes()], % Actualiza la lista de usuarios de todos los nodos
						Who!{ok}, checkuser([User | UserList]) % Avisa que el usuario fue agregado correctamente
				end
		end.

gamelist(GameList) ->
  receive
    % Imprime por terminal la lista de juegos
	{print} -> io:format(">> Lista de partidas: ~p", [GameList]), gamelist(GameList);

    % Informa al jugador la lista de juegos
	{print, Who} ->
	    Who!{lsg, [{X,Y} || {X,Y,_,_,_,_} <- GameList]},
	    gamelist(GameList);

    {newnode, Local, Visitante, Tablero, Observadores, LocalId, VisId} ->
        gamelist([{Local, Visitante, Tablero, Observadores, LocalId, VisId} | GameList]); 
	
	% Agrega un juego a la lista
	{new, Local, Visitante, Tablero, Observadores, Who} ->
	  Members = [ {A,B,C,D,E,F} || {A,B,C,D,E,F} <- GameList, (A == Local) or (B == Local) ],
	  if
	    Members == [] ->
	        [{gamelist, Node}!{newnode, Local, Visitante, Tablero, Observadores, Who, empty} || Node <- nodes()],
            Who!{new, ok}, % Avisa que salio todo bien
            gamelist([{Local, Visitante, Tablero, Observadores, Who, empty} | GameList]);
        true -> Who!{new,error}, gamelist(GameList)
      end;
  	
  	% Actualiza la lista cuando se une un observador
  	{newobs, NewList} -> gamelist(NewList);
  	
  	% Intenta unirse un observador
  	{obs, GameId, Username, Who} -> % PUEDE OBSERVAR VARIAS VECES
  	    Members = [ {A,B,C,D,E,F} || {A,B,C,D,E,F} <- GameList, A == GameId, A /= Username, B /= Username ],
  	    if
  	        Members /= [] ->
  	            Who!{obs, ok},
  	            NewList = [{A,B,C, case A of GameId -> [Who] ++ D; _ -> D end,E,F} || {A,B,C,D,E,F} <- GameList],
  	            [{gamelist, Node}!{newobs, NewList} || Node <- nodes()],
  	            gamelist(NewList);
  	        true -> Who!{obs,error}, gamelist(GameList)
  	    end;
  	  
  	% Actualiza la lista cuando se une un jugador.
  	{actlist, NewList} -> gamelist(NewList);
  	
  	% Intenta unirse a una partida.
  	{acc, GameId, Username, Who} ->
  	    Members = [ {A,B,C,D,E,F} || {A,B,C,D,E,F} <- GameList, A == GameId, B == empty, A /= Username ],
  	    if
  	        Members /= [] ->
  	            Who!{acc, ok},
  	            NewList = [{A, case A of GameId -> Username; _ -> B end,C,D,E, Who} || {A,B,C,D,E,_} <- GameList],
  	            [{gamelist, Node}!{actlist, NewList} || Node <- nodes()],
  	            gamelist(NewList);
  	        true -> Who!{acc,error}, gamelist(GameList)
  	    end
  end.

dispatcher(ListenSocket) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	io:format(">> Nuevo cliente: ~p.~n", [Socket]),
	spawn(?MODULE, psocket, [Socket, Socket]),
	dispatcher(ListenSocket).

% Recibe los pedidos del cliente y se los encarga al servidor con menos carga.
psocket(Socket, Username) ->
	{Atomo, CMD} = gen_tcp:recv(Socket, 0),
	if
		Atomo /= ok -> io:format(">> Se ha producido un error en el cliente ~p. Conexion cerrada.~n", [Username]);
		true ->
			pbalance!{self(), where}, % Pregunta a pbalance en que servidor crear el pcomando
			receive
				{BestNode} ->
					% io:format(">> Usuario ~p ejecutado comando ~p en ~p.~n", [Username, CMD, BestNode]),
					spawn(BestNode, ?MODULE, pcomando, [Socket, Username, CMD, self()]), % Crea el pcomando en el servidor correspondiente
					
					% Espera la respuesta de pcomando
					receive
						{con, error} -> gen_tcp:send(Socket, ">> Usted ya ha elegido un nombre de usuario."), psocket(Socket, Username);
						{con, User} -> gen_tcp:send(Socket, ">> Nombre de usuario aceptado.\n"), psocket(Socket, User);
						{lsg, GameList} ->
						    R = io_lib:format("~p ~n",[GameList]), lists:flatten(R), 
						    gen_tcp:send(Socket, [">> Lista de juegos: " | R]),
						    psocket(Socket, Username);
						{new, ok} -> gen_tcp:send(Socket, ">> Partida creada corectamente.\n"), psocket(Socket,Username);
						{new, error} -> gen_tcp:send(Socket, ">> Error: usted ya es miembro de una partida.\n"), psocket(Socket,Username);
						{acc, ok} -> gen_tcp:send(Socket, ">> Usted se ha unido a la partida.\n"), psocket(Socket,Username);
						{acc, error} -> gen_tcp:send(Socket, ">> Error: no se ha podido unir a la partida.\n"), psocket(Socket,Username);
    					{obs, ok} -> gen_tcp:send(Socket, ">> Usted esta observando la partida.\n"), psocket(Socket,Username);
						{obs, error} -> gen_tcp:send(Socket, ">> Error: no se puede observar esa partida.\n"), psocket(Socket,Username);
						{_, _} -> gen_tcp:send(Socket, [">> Comando no programado.\n"]), psocket(Socket, Username)
					end
			end
	end.

% Realiza los pedidos del cliente.
pcomando(Socket, Username, CMD, Who) ->
	% io:format(">> Servidor ~p ejecutado comando ~p a peticion de ~p.~n", [node(), CMD, Username]),
	case string:tokens(string:strip(string:strip(CMD, right, $\n),right,$\r), " ") of
		["CON", User] ->
			if
				Socket /= Username -> Who!{con, error};
				true ->
					{checkuser, node()}!{self(), User},
					receive
						{ok} -> io:format(">> Cliente ~p ahora se llama ~p.~n", [Socket, User]), Who!{con, User};
						_ -> Who!{con, Username}
					end
			end;
		["LSG"] -> {gamelist, node()}!{print, Who};
		["NEW"] ->
		    {gamelist, node()}!{new, Username, empty, ?TABLEROINICIAL, [], Who};
		["OBS", GameId] -> gamelist!{obs, GameId, Username, Who};
		["ACC", GameId] -> gamelist!{acc, GameId, Username, Who};
		_ -> Who!{error, nocmd}
	end.

pstat() ->
	% Envia la carga actual, a los pbalance de todos los nodos
	[{pbalance, Node}!{node(), load()} || Node <- [node() | nodes()]],

	% Espera 10 segundos
	timer:sleep(10000),

	% Comienza otra vez
	pstat().

pbalance(LoadList) ->
	receive
		% Imprime por terminal la carga de los nodos
		{print} -> io:format("Balance de cargas: ~p", [LoadList]), pbalance(LoadList);

		% Responde a un psocket, cual es el servidor con menor carga
		{Who, where} -> {BestNode, _} = lists:nth(1, lists:keysort(2, LoadList)), Who!{BestNode}, pbalance(LoadList);

		% Recibe la informacion de un pstat y actualiza la lista
		{Node, Load} -> pbalance([{X,case X of Node -> Load; _ -> Y end} || {X,Y} <- LoadList])
	end.