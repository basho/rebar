
.PHONY: all clean install

all: build

build: rebar

rebar: bootstrap 
	./bootstrap

clean:
	rm -rf rebar ebin/*.beam inttest/rt.work

DESTDIR ?= ""

install: build
	install -D -o root -g root -m 755 ./rebar $(DESTDIR)/usr/bin/rebar
