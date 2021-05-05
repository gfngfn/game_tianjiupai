#!/bin/bash

USER_ID="$1"
SESSION="$2"
ROOM_ID="$3"
VERBOSE="$4"

HOST="localhost:8080"
URI="http://$HOST/rooms/$ROOM_ID"
BODY="{\"user_id\": \"$USER_ID\"}"

if [ "$VERBOSE" = "" ]; then
  curl -X PUT -H "Content-Type: application/json" -H "Cookie: session=$SESSION" -d "$BODY" "$URI"
else
  curl -v -X PUT -H "Content-Type: application/json" -H "Cookie: session=$SESSION" -d "$BODY" "$URI"
fi
