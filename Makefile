
.PHONY: all
all:
	mkdir -p public/assets
	./compile_client.sh
	rebar3 compile
