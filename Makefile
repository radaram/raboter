REBAR?=rebar


all: build


clean:
	$(REBAR) clean

deps:
	$(REBAR) get-deps

build: deps
	$(REBAR) compile


