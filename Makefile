all: get-deps compile

get-deps:
	./rebar get-deps

compile:
	./rebar compile

run:
	erl -pa ebin/ -eval "application:start(tellstick)"


