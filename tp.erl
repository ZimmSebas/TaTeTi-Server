% El cliente (cliente) llega a la puerta del bar (servidor) en donde es atendido por el recepcionista (dispatcher). Este lo dirige a la mesa en donde sera antendido por un mozo (psocket).
% El cliente le indica su pedido al mozo, y este se dirige a hablar con el supervisor (pbalance). El supervisor le indica al mozo cual es la sucursal con menor carga de trabajo.
% El mozo se dirige a la sucursal indicada por su supervisor y le solicita al cocinero (pcomando) el pedido del cliente. Una vez listo el pedido, el cocinero le notifica al mozo, el cual a su vez notifica al cliente.
% Notese que para que el supervisor pueda indicarle al mozo cual es la sucursal con menor carga de trabajo, es necesario que se mantenga en contacto con los telefonistas (pstat) de todas las sucursales.
% Estos se encargan de informar a los supervisores de las diferentes sucursales, cual es la carga de trabajo del local.

% erl -name name@ip
-module(tp).
-define(SERVERS, ['nodo1@127.0.0.1','nodo2@127.0.0.1', 'nodo3@127.0.0.1']).
-define(LOADS, [999, 999, 999]).
-export([init/0, load/0, transpose/1, winner/1, checkuser/1, dispatcher/1, gamelist/1, psocket/2, pcomando/4, pstat/0, pbalance/1]).
%-compile(export_all).

load() -> length(erlang:ports()).

transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].
  
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
		{print, Who} -> Who!{lsg, GameList};
    
    {newnode, Gameid, User1} ->
      gamelist([{Gameid, User1, empty} | GameList]);
		
		% Agrega un juego a la lista
		{new, Gameid, User1} ->
		  [{gamelist, Node}!{newnode, Gameid, User1} || Node <- nodes()],
		  gamelist([{Gameid, User1, empty} | GameList]);
		
		% Se une un jugador a una partida existente
		{add, Gameid, User2} -> gamelist([{X,Y, case X of Gameid -> User2; _ -> Z end} || {X,Y,Z} <- GameList]) 
	end.

dispatcher(ListenSocket) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	io:format(">> Nuevo cliente: ~p.~n", [Socket]),
	spawn(?MODULE, psocket, [Socket, Socket]),
	dispatcher(ListenSocket).

psocket(Socket, Username) ->
	{Atomo, CMD} = gen_tcp:recv(Socket, 0),
	if
		Atomo /= ok -> io:format(">> Se ha producido un error en el cliente ~p. Conexion cerrada.~n", [Username]);
		true ->
			pbalance!{self(), where}, % Pregunta a pbalance en que servidor crear el pcomando
			receive
				{BestNode} ->
					io:format(">> Usuario ~p ejecutado comando ~p en ~p.~n", [Username, CMD, BestNode]),
					spawn(BestNode, ?MODULE, pcomando, [Socket, Username, CMD, self()]), % Crea el pcomando en el servidor correspondiente
					receive
						{con, error} -> gen_tcp:send(Socket, [">> Usted ya ha elegido un nombre de usuario: " | Username]), psocket(Socket, Username);
						{con, User} -> gen_tcp:send(Socket, ">> Nombre de usuario aceptado.\n"), psocket(Socket, User);
						{lsg, GameList} -> gen_tcp:send(Socket, [">> Lista de juegos: " | GameList]), psocket(Socket, Username);
						{_, _} -> gen_tcp:send(Socket, [">> Comando no programado.\n"]), psocket(Socket, Username)
					end
			end
	end.

pcomando(Socket, Username, CMD, Who) ->
	io:format(">> Servidor ~p ejecutado comando ~p a peticion de ~p.~n", [node(), CMD, Username]),
	case string:tokens(CMD, " ") of
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
		["LSG", _] ->
		  gamelist!{print, Who};
		["NEW", Gameid] ->
		  gamelist!{new, Gameid, Username};
		[_, _] -> Who!{error, nocmd}
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
