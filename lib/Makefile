.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = lib_md5 lib_chan_auth lib_chan_mm lib_chan_cs \
	lib_chan \
	any_apply test1 mod_srpc mod_echo mod_math \
	lib_chan_test

all: compile

compile: ${MODS:%=%.beam}

test_server: compile
	${ERL} -pa '.' -s lib_chan_test start_server 'test.conf'

test_client: compile
	${ERL} -pa '.' -s lib_chan_test start_client

clean:	
	rm -rf *.beam erl_crash.dump

