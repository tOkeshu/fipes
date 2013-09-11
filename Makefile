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

install: app
	mkdir -p /usr/lib/fipes
	cp -r rel/* /usr/lib/fipes/
	mkdir -p /etc/fipes
	ln -s /usr/lib/fipes/fipes/etc/app.config /etc/fipes/
	ln -s /usr/lib/fipes/fipes/bin/fipes /usr/bin/
