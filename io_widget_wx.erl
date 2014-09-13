%% ---
%% io_widget の wx 実装
%% ---
-module(io_widget_wx).

-export([get_state/1,
	 start/1,
         test/0, 
	 set_handler/2, 
	 set_prompt/2,
	 set_state/2,
	 set_title/2,
         insert_str/2,
         update_state/3]).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/src/wxe.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 外部インターフェース
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% GUI起動
start(Pid) ->
    spawn_link(fun() -> widget(Pid) end).

%% 状態の取得（同期呼び出し）
get_state(Pid)          -> rpc(Pid, get_state).

%% タイトル設定
set_title(Pid, Str)     -> Pid ! {title, Str}.

%% 入力ハンドラ（パーサ）関数設定
set_handler(Pid, Fun)   -> Pid ! {handler, Fun}.

%% プロンプト設定
set_prompt(Pid, Str)    -> Pid ! {prompt, Str}.

%% 状態を設定
set_state(Pid, State)   -> Pid ! {state, State}.

%% 出力ペイン(editor?)に追記
insert_str(Pid, Str)    -> Pid ! {insert, Str}.

%% 状態を更新 （状態タプルの N 要素目を X に）
update_state(Pid, N, X) -> Pid ! {updateState, N, X}. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 内部実装
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ウィジェットID
-define(FRAME, 0).
-define(INPUT_TEXT, 1).
-define(OUTPUT_TEXT, 2).

%% RPCヘルパー
rpc(Pid, Q) ->    
    Pid ! {self(), Q},
    receive
	{Pid, R} ->
	    R
    end.

%% GUIウィジェット初期化
widget(Pid) ->
    Wx = wx:new(),
    Size = {500, 200},

    F  = wxFrame:new(Wx, ?FRAME, "window", [{size, Size}]),

    Panel = wxPanel:new(F, []),
    
    % Output (multiline)
    OutputText = wxTextCtrl:new(Panel, ?OUTPUT_TEXT,
                                [{value, ""},
                                 {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
    wxTextCtrl:setEditable(OutputText,false),

    % Input (singleline)
    InputText  = wxTextCtrl:new(Panel, ?INPUT_TEXT, [{value, ""},
                                                     {style, ?wxDEFAULT}]),
    %% GUIイベントに接続
    %% connect すると self() にメッセージが届くようになる
    wxTextCtrl:connect(InputText, char, [{skip, true}]),
    %% メッセージではなく関数で受けるなら、たとえば次のようにすればいい
    %% 暗黙のskipを無効にしたうえでハンドラ内から明示的にskipする例
    % OnChar = fun(Source, Event) ->
    %                  io:format("OnChar: ~tp - ~tp~n", [Source, Event]),
    %                  wxEvent:skip(Event)
    %          end,
    % wxTextCtrl:connect(InputText, char, [{callback, OnChar}]),
    wxFrame:connect(F, close_window, [{skip, true}]),

    %% Sizer
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    OutputSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Output"}]),
    InputSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Say"}]),

    wxSizer:add(InputSizer,  InputText,  [{flag, ?wxEXPAND}]),
    wxSizer:add(OutputSizer, OutputText, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxSizer:add(MainSizer, OutputSizer, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:addSpacer(MainSizer, 10), % Spacer
    wxSizer:add(MainSizer, InputSizer,  [{flag, ?wxEXPAND}]),

    wxPanel:setSizer(Panel, MainSizer),

    wxFrame:show(F), % ウィンドウ表示

    State = nil,

    Prompt = " > ",

    loop(F, Pid, Prompt, State, fun parse/1). 

%% 出力ペインを最終行までスクロール
scroll_to_show_last_line(F) ->
    T = wx:typeCast(wxTextCtrl:findWindow(F, ?OUTPUT_TEXT), wxTextCtrl),
    Range = wxTextCtrl:getScrollRange(T, 1),
    wxTextCtrl:setScrollPos(T, 1, Range),
    ok.

%% 文字列パーサー
parse(Str) ->
    {str, Str}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% メッセージループ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(F, Pid, Prompt, State, Parse) ->   
    receive
	{From, get_state} ->
	    From ! {self(), State},
	    loop(F, Pid, Prompt, State, Parse);
	{handler, Fun} ->
	    loop(F, Pid, Prompt, State, Fun);
	{prompt, Str} ->
            T = wx:typeCast(wxTextCtrl:findWindow(F, ?INPUT_TEXT), wxTextCtrl),
            wxTextCtrl:setValue(T, Str),
            wxTextCtrl:setInsertionPointEnd(T),
	    loop(F, Pid, Str, State, Parse);
	{state, S} ->
	    loop(F, Pid, Prompt, S, Parse);
	{title, Str} ->
            wxFrame:setTitle(F, Str),
	    loop(F, Pid, Prompt, State, Parse);
	{insert, Str} ->
            T = wx:typeCast(wxTextCtrl:findWindow(F, ?OUTPUT_TEXT), wxTextCtrl),
            wxTextCtrl:appendText(T, Str),
	    scroll_to_show_last_line(F),
	    loop(F, Pid, Prompt, State, Parse);
	{updateState, N, X} ->
	    io:format("setelemtn N=~p X=~p Satte=~p~n",[N,X,State]),
	    State1 = setelement(N, State, X), % タプルStateのN番目をXに
	    loop(F, Pid, Prompt, State1, Parse);
        #wx{id = ?INPUT_TEXT, event = #wxKey{ keyCode = ?WXK_RETURN }} ->
            T = wx:typeCast(wxTextCtrl:findWindow(F, ?INPUT_TEXT), wxTextCtrl),
            Text = wxTextCtrl:getValue(T),
            wxTextCtrl:setValue(T, Prompt),
            wxTextCtrl:setInsertionPointEnd(T),
	    % io:format("Read:~tp~n",[Text]),
	    try Parse(Text) of
	       Term ->
	           Pid ! {self(), State, Term}
	    catch
	       _:_ ->
	           self() ! {insert, "** bad input**\n** /h for help\n"}
	    end,
	    loop(F, Pid, Prompt, State, Parse);
        #wx{id = ?INPUT_TEXT} ->
	    loop(F, Pid, Prompt, State, Parse);
        #wx{id = Id, event = #wxClose{type = close_window}} ->
	    io:format("Destroyed id=~p~n",[Id]),
	    exit(windowDestroyed);
	Any ->
	    io:format("Discarded:~p~n",[Any]),
	    loop(F, Pid, Prompt, State, Parse)
    end.

%%%%%%%%%%%%%%%%%%%%
%%% テスト
%%%%%%%%%%%%%%%%%%%%

test() ->
    spawn(fun() -> test1() end).

test1() ->
    W = io_widget_wx:start(self()),
    io:format("W = ~tp~n", [W]),
    io:format("self = ~tp~n", [self()]),
    io_widget_wx:set_title(W, "Test window"),
    loop(W).

loop(W) ->
    receive
	{W, {str, Str}} ->
	    Str1 = Str ++ "\n",
	    io_widget_wx:insert_str(W, Str1),
	    loop(W)
    end.

