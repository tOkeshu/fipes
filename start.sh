#!/bin/sh
erl -sname fipes -pa fipes/ebin -pa deps/*/ebin \
	-boot start_sasl -s fipes
