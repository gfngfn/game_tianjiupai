
# [WIP] A Tian Jiu Pai Game Server

A *Tian Jiu Pai* (*Tien Gow Pai*, 天九牌) game server written in [Sesterl](https://github.com/gfngfn/Sesterl), Erlang, and Elm.

The following is a capture video of playing the game:

![https://pbs.twimg.com/ext_tw_video_thumb/1404111611095252994/pu/img/yIbNyw8FmE__pZa9?format=jpg&name=small](https://twitter.com/bd_gfngfn/status/1404112257517195268?s=20)


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
