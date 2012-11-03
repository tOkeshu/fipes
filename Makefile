REBAR = ./rebar

all: compile

app: compile
	@$(REBAR) generate force=1

start: rel/fipes/bin/fipes
	@rel/fipes/bin/fipes start

stop: rel/fipes/bin/fipes
	@rel/fipes/bin/fipes stop

compile:
	@$(REBAR) get-deps compile

tests:
	@make -s start
	@casperjs test fipes/tests
	@make -s stop

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

dist-clean: clean

