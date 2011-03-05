all: get-deps
	./rebar compile

get-deps: 
	./rebar get-deps


clean:
	./rebar clean


release: clean all
	./rebar generate
