all:
	./rebar compile

clean:
	./rebar clean


release:
	./rebar clean compile generate
