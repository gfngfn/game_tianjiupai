#!/bin/bash

SRC="src_client/Main.elm"
TARGET="public/index.html"

elm make "$SRC" --output "$TARGET"
