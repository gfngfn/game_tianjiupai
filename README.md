
## Endpoints

### Register as a user

```
POST /users
  request body:
    {"user_name": <UserName/string>}
  response:
  - 201 Created
      {"user_id": <UserId/string>}
  resulting cookie:
    user_id
```


### Create a new room

```
POST /rooms
  request body:
    {"room_name": <RoomName/string>}
  sent cookie:
    user_id
  response:
  - 201 Created
      {"room_id": <RoomId/string>}
```


### Attend an existing room

```
PUT /rooms/<RoomId>
  sent cookie:
    user_id
  response:
  - 200 OK
```


## States

### Server

```
UserId := String
UserName := String
RoomId := String
RoomName := String

ServerState := {
  users : {UserId => UserName},
  rooms : {RoomId => Room},
}

Room := {
  name : RoomName,
  game : (waiting of Waiting | playing of Playing | finished),
}

Waiting := {
  entering : List<UserId>
  ready    : Set<UserId>
}

Playing := {
  members : {id : UserId, point : Int}^4,
  parent  : east | south | west | north,
  put     : (single of List<Card> | double of List<Card^2> | triple of List<Card^3> | quadruple of List<Card^4>),
  hands   : (List<Card>)^4
}
```

### Cookie

```
Cookie := {
  user_id : UserId
}
```
