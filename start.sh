#!/bin/sh
erl -sname fipes -pa /usr/lib/fipes/fipes/ebin -pa /usr/lib/fipes/deps/*/ebin \
	-boot start_sasl -s fipes
