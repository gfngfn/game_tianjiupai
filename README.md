
# [WIP] A Tian Jiu Pai Game Server

A *Tian Jiu Pai* (*Tien Gow Pai*, 天九牌) game server written in [Sesterl](https://github.com/gfngfn/Sesterl), Erlang, and Elm.


## Memos for development

### Build dependencies

* make
* Erlang/OTP
* Rebar3
* Elm
* [APBuf](https://github.com/gfngfn/apbuf)
  - Used for generating a JSON encoder/decoder from `model.apbuf`.
  - Can be intalled via OPAM pin.
* [Sesterl](https://github.com/gfngfn/Sesterl)
  - A statically-typed Erlang.
  - Can be intalled via OPAM pin.


### How to build

Just invoke `make`.


### How to run tests

Invoke `make && rebar3 eunit`.


### How to launch locally

Invoke `make && rebar3 shell`.
