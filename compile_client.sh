#!/bin/bash

SRC="src_client/Main.elm"
TARGET="public/assets/main.js"

elm make "$SRC" --output "$TARGET"
