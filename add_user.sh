#!/bin/bash

if [ $# -ne 2 ]; then
  echo "Usage: $0 <VERBOSE> <HOST>"
  exit 1
fi

VERBOSE="$1"

HOST="$2"
URI="http://$HOST/users"
BODY='{"user_name": "Taro"}'

if [ "$VERBOSE" = "" ]; then
  curl -X POST -H "Content-Type: application/json" -d "$BODY" "$URI" | jq -r '.user_id'
else
  curl -v -X POST -H "Content-Type: application/json" -d "$BODY" "$URI" | jq -r '.user_id'
fi
