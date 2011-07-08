VERSION=R1.1

all: get-deps
	./rebar compile

get-deps: 
	./rebar get-deps


clean:
	./rebar clean


release: clean all
	./rebar generate
	mv ./rel/mmyn ./rel/mmyn-$(VERSION)
	cd rel && tar -czvf mmyn-$(VERSION).tar.gz mmyn-$(VERSION) && cd ..
	mv ./rel/mmyn-$(VERSION).tar.gz mmyn-$(VERSION).tar.gz
