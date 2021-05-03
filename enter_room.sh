#!/bin/bash

USER_ID=$1
ROOM_ID=$2

curl -v -X PUT -H "Content-Type: application/json" -d "{\"user_id\": \"$USER_ID\"}" "http://localhost:8080/rooms/$ROOM_ID"
