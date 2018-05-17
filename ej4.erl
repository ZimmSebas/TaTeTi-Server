-module(ej4).
-compile(export_all).

procesos() ->
	receive
		{pid, Pid} -> 
			procesos_no_init(Pid)
	end,
	ok.

procesos_no_init(Proximo) ->
  receive 
    {Msg, N} -> 
      if
        N > 0 ->
          Proximo!{Msg, N-1},
          io:format("~p ~p~n",[Msg, N]),
          procesos_no_init(Proximo);
        true ->
          Proximo!{'exit'},
          procesos_no_init(Proximo)
      end;
    {exit} ->
      io:format("Nos vimo.~n"),
      Proximo!{exit},
      exit("Algo")
  end,
  ok.

lanzarprocesos(N, Pid) ->
  PidProx = spawn(?MODULE, procesos,[]),
  if
    N > 1 ->
      PidProx!{pid, lanzarprocesos(N-1,Pid)};
  true ->
		PidProx!{pid, Pid}
  end,
  PidProx.


init(N) ->
  PidN = spawn(?MODULE, procesos,[]),
  Pid1 = lanzarprocesos(N-1, PidN),
  %~ io:format("~p~n",[Pid1]),
  PidN!{pid,Pid1},
  %~ io:format("~p~n",[PidN]),
    
  Pid1!{"Pepe",N/2}. %jiji
