#!/bin/bash

USER_ID=$1
ROOM_ID=$2

HOST="localhost:8080"
URI="http://$HOST/rooms/$ROOM_ID"
BODY="{\"user_id\": \"$USER_ID\"}"

if [ "$3" = "" ]; then
  curl -X PUT -H "Content-Type: application/json" -d "$BODY" "$URI"
else
  curl -v -X PUT -H "Content-Type: application/json" -d "$BODY" "$URI"
fi
