
## Endpoints

### Register as a user

```
POST /users
  create_user_request := {
    user_name : string,
  }
<--- 201 Created
  create_user_response := {
    user_id : user_id,
  }
```


### Create a new room

```
POST /rooms
  create_room_request := {
    room_name : string,
  }
<--- 201 Created
  create_room_response := {
    room_id : room_id,
  }
```


### Attend an existing room

```
PATCH /rooms/<room_id>
  EnterRoom{
    user_id : user_id,
  }
<--- 200 OK
  enter_room_response := personal_state
```


### Post a comment

```
PATCH /rooms/<room_id>
  Comment{
    user_id : user_id,
    text    : string,
  }
<--- 200 OK
```


### Send Ack

```
PATCH /rooms/<room_id>
  Ack{
    user_id     : user_id,
    snapshot_id : snapshot_id,
  }
<--- 200 OK
```


### Submit card(s)

```
PATCH /rooms/<room_id>
  Submit{
    user_id : user_id,
    cards   : list(card),
  }
<--- 201 Created
  {
    observable_game_state : observable_game_state,
  }
```


### Get the current list of rooms

```
GET /rooms
<--- 200 OK
  get_all_rooms_response := {
    rooms : list(room_summary)
  }
```


### Get the current personal state of a room

```
GET /rooms/<room_id>/users/<user_id>
<--- 200 OK
  get_room_response := personal_state
```


### Get the page

```
GET /
<--- 200 OK
  (HTML)
```
