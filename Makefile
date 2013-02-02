REBAR = ./rebar

all: compile

app: compile
	@$(REBAR) generate force=1

compile:
	@$(REBAR) get-deps compile

tests: compile
	@$(REBAR) eunit

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

dist-clean: clean

install: compile
	mkdir -p /usr/lib/fipes
	cp -r fipes /usr/lib/fipes/
	cp -r deps /usr/lib/fipes/
