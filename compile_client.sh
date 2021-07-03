#!/bin/bash

SRC="src_client/Main.elm"
TARGET="priv/public/assets/main.js"

elm make "$SRC" --output "$TARGET"
