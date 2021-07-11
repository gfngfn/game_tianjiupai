ASSETS_TARGET_DIR=priv/public/assets

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
	mkdir -p src/_generated
	apbuf model.apbuf

.PHONY: public-assets
public-assets:
	mkdir -p $(ASSETS_TARGET_DIR)
	cp assets_client/* $(ASSETS_TARGET_DIR)

.PHONY: test
test: server
	rebar3 eunit

.PHONY: run
run: client server
	rebar3 shell
