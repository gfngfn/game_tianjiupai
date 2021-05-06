#!/bin/bash

VERBOSE="$1"

HOST="localhost:8080"
URI="http://$HOST/users"
BODY='{"user_name": "Taro"}'

if [ "$VERBOSE" = "" ]; then
  curl -X POST -H "Content-Type: application/json" -d "$BODY" "$URI" | jq -r '.user_id'
else
  curl -v -X POST -H "Content-Type: application/json" -d "$BODY" "$URI" | jq -r '.user_id'
fi
