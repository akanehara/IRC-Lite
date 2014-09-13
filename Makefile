# 以下の行はそのままにしておくこと
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

# コンパイルしたいErlangモジュールのリストをここに書く
# モジュールのリストが1行に収まらない場合は行の最後に \ 文字
# を置いて次の行に続ける

# 以下の行を編集すること
MODS = io_widget_wx io_widget chat_client \
       mod_chat_controller chat_group chat_server

# Makefileの最初のターゲットがデフォルトのターゲットになる
# 単に「make」と入力すると、「make all」が仮定される
# (このMakefileでは「all」が最初のターゲットであるため)

all: compile

compile: ${MODS:%=%.beam} subdirs
	
## 特別なコンパイル指定はここに追加する
# special1.beam: special1.erl    
#	${ERL} -Dflag1 -W0 special1.erl

## Makefileからアプリケーションを実行する

chat_client: compile
	${ERL} -pa lib -s chat_client test

chat_server: compile
	${ERL} -pa lib -s chat_server start

# subdirsターゲットはサブディレクトリにあるコードを
# コンパイルする

subdirs:
	cd lib; make

# コンパイルされたコードをすべて削除する

clean:	
	rm -rf *.beam erl_crash.dump
	cd lib; make clean

