%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%  
%%  日本語版の『プログラミングErlang』、およびサンプルコードについては、
%%   http://ssl.ohmsha.co.jp/cgi-bin/menu.cgi?ISBN=978-4-274-06714-3 を確認してください。
%% ---
-module(lib_chan_cs).
%% csはclient_serverの略

-export([start_raw_server/4, start_raw_client/3]).
-export([stop/1]).
-export([children/1]).


%% start_raw_server(Port, Fun, Max)
%%   このサーバはPortにおいて最大Max個までの接続を受け付ける
%%   Portに対して*最初に*接続が行われたときにFun(Socket)が呼び出される。
%%   その後はソケットへのメッセージはハンドラへのメッセージになる。

%% tcpは以下のように使われるのが普通だ：
%% 待ち受け(リスナ)を用意するには
%%   start_agent(Port) ->    
%%     process_flag(trap_exit, true),
%%     lib_chan_server:start_raw_server(Port, 
%% 		         	       fun(Socket) -> input_handler(Socket) end, 
%% 				       15,
%%                                     0).

start_raw_client(Host, Port, PacketLength) ->
    gen_tcp:connect(Host, Port,
		    [binary, {active, true}, {packet, PacketLength}]).

%% start_raw_serverから戻ってきたときには即座に接続を受け付ける
%% 準備ができているはず

start_raw_server(Port, Fun, Max, PacketLength) ->
    Name = port_name(Port),
    case whereis(Name) of
	undefined ->
	    Self = self(),
	    Pid = spawn_link(fun() ->
				 cold_start(Self,Port,Fun,Max,PacketLength)
			     end),
	    receive
		{Pid, ok} ->
		    register(Name, Pid),
		    {ok, self()};
		{Pid, Error} ->
		    Error
	    end;
	_Pid ->
	    {error, already_started}
    end.

stop(Port) when integer(Port) ->
    Name = port_name(Port),
    case whereis(Name) of
	undefined ->
	    not_started;
	Pid ->
	    exit(Pid, kill),
	    (catch unregister(Name)),
	    stopped
    end.
children(Port) when integer(Port) ->
    port_name(Port) ! {children, self()},
    receive
	{session_server, Reply} -> Reply
    end.


port_name(Port) when integer(Port) ->
    list_to_atom("portServer" ++ integer_to_list(Port)).


cold_start(Master, Port, Fun, Max, PacketLength) ->
    process_flag(trap_exit, true),
    %% io:format("Starting a port server on ~p...~n",[Port]),
    case gen_tcp:listen(Port, [binary,
			       %% {dontroute, true},
			       {nodelay,true},
			       {packet, PacketLength},
			       {reuseaddr, true}, 
			       {active, true}]) of
	{ok, Listen} ->
	    %% io:format("Listening to:~p~n",[Listen]),
	    Master ! {self(), ok},
	    New = start_accept(Listen, Fun),
	    %% 処理の本体に移る準備ができた
	    socket_loop(Listen, New, [], Fun, Max);
	Error ->
	    Master ! {self(), Error}
    end.


socket_loop(Listen, New, Active, Fun, Max) ->
    receive
	{istarted, New} ->
	    Active1 = [New|Active],
	    possibly_start_another(false,Listen,Active1,Fun,Max);
	{'EXIT', New, _Why} ->
	    %% io:format("Child exit=~p~n",[Why]),
	    possibly_start_another(false,Listen,Active,Fun,Max);
	{'EXIT', Pid, _Why} ->
	    %% io:format("Child exit=~p~n",[Why]),
	    Active1 = lists:delete(Pid, Active),
	    possibly_start_another(New,Listen,Active1,Fun,Max);
	{children, From} ->
	    From ! {session_server, Active},
	    socket_loop(Listen,New,Active,Fun,Max);
	_Other ->
	    socket_loop(Listen,New,Active,Fun,Max)
    end.


possibly_start_another(New, Listen, Active, Fun, Max) 
  when pid(New) ->
    socket_loop(Listen, New, Active, Fun, Max);
possibly_start_another(false, Listen, Active, Fun, Max) ->
    case length(Active) of
	N when N < Max ->
	    New = start_accept(Listen, Fun),
	    socket_loop(Listen, New, Active, Fun,Max);
	_ ->
	    socket_loop(Listen, false, Active, Fun, Max)
    end.

start_accept(Listen, Fun) ->
    S = self(),
    spawn_link(fun() -> start_child(S, Listen, Fun) end).

start_child(Parent, Listen, Fun) ->
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    Parent ! {istarted,self()},		    % コントローラに通知する
	    inet:setopts(Socket, [{packet,4},
				  binary,
				  {nodelay,true},
				  {active, true}]), 
	    %% ソケットを有効にする前
	    %% io:format("running the child:~p Fun=~p~n", [Socket, Fun]),
	    process_flag(trap_exit, true),
	    case (catch Fun(Socket)) of
		{'EXIT', normal} ->
		    true;
		{'EXIT', Why} ->
		    io:format("Port process dies with exit:~p~n",[Why]),
		    true;
		_ ->
		    %% 終了以外ならば問題なし
		    true
	    end
    end.


