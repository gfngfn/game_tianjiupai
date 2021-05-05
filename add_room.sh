#!/bin/bash

USER_ID="$1"
SESSION="$2"
VERBOSE="$3"

HOST="localhost:8080"
URI="http://$HOST/rooms"
BODY="{\"user_id\": \"$USER_ID\", \"room_name\": \"bar\"}"

if [ "$VERBOSE" = "" ]; then
  curl -X POST -H "Content-Type: application/json" -H "Cookie: session=$SESSION" -d "$BODY" "$URI"
else
  curl -v -X POST -H "Content-Type: application/json" -H "Cookie: session=$SESSION" -d "$BODY" "$URI"
fi
