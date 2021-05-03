#!/bin/bash

curl -v -X POST -H "Content-Type: application/json" -d '{"user_name": "Taro"}' "http://localhost:8080/users"
