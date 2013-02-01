REBAR = rebar

all: compile

start:
	@start

stop:
	@stop

compile:
	@$(REBAR) get-deps compile

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

dist-clean: clean

