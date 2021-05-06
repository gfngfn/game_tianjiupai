#!/bin/bash

USER_ID="$1"
SESSION="$2"
VERBOSE="$3"

HOST="localhost:8080"
URI="http://$HOST/rooms"
BODY="{\"user_id\": \"$USER_ID\", \"room_name\": \"bar\"}"
HA="Content-Type: application/json"
HB="Cookie: session=$SESSION"

if [ "$VERBOSE" = "" ]; then
  curl -X POST -H "$HA" -H "$HB" -d "$BODY" "$URI" | jq -r '.room_id'
else
  curl -v -X POST -H "$HA" -H "$HB" -d "$BODY" "$URI" | jq -r '.room_id'
fi
