VERSION=R1.2

all: get-deps
	./rebar compile

get-deps: 
	./rebar get-deps


clean:
	./rebar clean
	rm -rf ./rel/mmyn-$(VERSION)
	rm -rf mmyn-$(VERSION)


release: clean all
	./rebar generate
	mv ./rel/mmyn ./rel/mmyn-$(VERSION)
	cd rel && tar -czvf mmyn-$(VERSION).tar.gz mmyn-$(VERSION) && cd ..
	mv ./rel/mmyn-$(VERSION).tar.gz mmyn-$(VERSION).tar.gz
