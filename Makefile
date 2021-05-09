
.PHONY: all
all:
	mkdir -p src_client/_generated
	mkdir -p public/assets
	apbuf model.apbuf
	./compile_client.sh
	rebar3 compile
