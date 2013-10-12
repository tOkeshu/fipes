PROJECT = fipes

DEPS = cowboy tnetstrings
dep_cowboy = https://github.com/extend/cowboy.git 0.8.6
dep_tnetstrings = https://github.com/tOkeshu/tnetstrings.git tmp/fipes

include erlang.mk

start:
	erl -sname fipes@localhost -pa ebin -pa deps/*/ebin -boot start_sasl -s fipes -detached

dev:
	erl -sname fipes@localhost -pa ebin -pa deps/*/ebin -boot start_sasl -s fipes

stop:
	erl -sname fipectl@localhost -pa ebin -pa deps/*/ebin -eval "fipes:shutdown(fipes@localhost)" -s init stop -detached

