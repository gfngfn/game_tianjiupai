#!/bin/bash

curl -v -X POST -H "Content-Type: application/json" -d '{"room_name": "bar"}' "http://localhost:8080/rooms"
