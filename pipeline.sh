#!/bin/bash
set -xe

BUILD_ROOT_PATH="home/ubuntu"
PROJECT_PATH="${BUILD_ROOT_PATH}/game_tianjiupai"
PROJECT_URL="https://github.com/gfngfn/game_tianjiupai"

REBAR3_URL="https://s3.amazonaws.com/rebar3/rebar3"

echo "==== Installing Rebar3 ===="
wget "${REBAR3_URL}"
mv rebar3 /usr/bin

echo "==== Clone the repository ===="
git -C "${BUILD_ROOT_PATH}" clone "${PROJECT_URL}"

echo "==== Releasing the project ===="
cd "${PROJECT_PATH}" && rebar3 sesterl compile && rebar3 as prod release
chown -R ubuntu:ubuntu "${PROJECT_PATH}"

echo "==== Starting ===="
systemctl enable erlang-app
