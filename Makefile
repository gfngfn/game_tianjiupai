
.PHONY: all
all: client server

.PHONY: server
server:
	rebar3 sesterl compile

.PHONY: client
client: model public-assets
	./compile_client.sh

.PHONY: model
model:
	mkdir -p src_client/_generated
	apbuf model.apbuf

.PHONY: public-assets
public-assets:
	mkdir -p public/assets
	cp assets_client/* public/assets

.PHONY: test
test: server
	rebar3 eunit

.PHONY: run
run: client server
	rebar3 shell
