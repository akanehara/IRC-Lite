all: clean compile

ERL = erl

compile: subdirs
	$(ERL) -make

clean:
	rm -rf ./ebin/*.beam erl_crash.dump

subdirs:
	cd lib_chan; make

chat_client: compile
	${ERL} -pa './ebin' -pa './lib_chan/ebin' -s chat_client test

chat_server: compile
	${ERL} -pa './ebin' -pa './lib_chan/ebin' -s chat_server start

