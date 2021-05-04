#!/bin/bash

HOST="localhost:8080"
URI="http://$HOST/rooms"
BODY='{"room_name": "bar"}'

if [ "$1" = "" ]; then
  curl -X POST -H "Content-Type: application/json" -d "$BODY" "$URI"
else
  curl -v -X POST -H "Content-Type: application/json" -d "$BODY" "$URI"
fi
