
.PHONY: all
all: model client server

.PHONY: server
server:
	mkdir -p src_client/_generated
	rebar3 sesterl compile

.PHONY: model
model:
	apbuf model.apbuf

.PHONY: client
client:
	mkdir -p public/assets
	cp assets_client/* public/assets
	./compile_client.sh

.PHONY: test
test: server
	rebar3 eunit
