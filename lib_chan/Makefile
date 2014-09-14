all: clean compile

ERL = erl

compile:
	$(ERL) -make

clean:
	rm -rf ./ebin/*.beam erl_crash.dump

test_server: compile
	${ERL} -boot start_clean -pa './ebin' -s lib_chan_test start_server 'test.conf'

test_client: compile
	${ERL} -boot start_clean -pa './ebin' -s lib_chan_test start_client

