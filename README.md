
# A Tian Jiu Pai Game Server

![title image](https://github.com/gfngfn/game_tianjiupai/blob/master/assets_client/top.png)

A *Tian Jiu Pai* (*Tien Gow Pai*, 天九牌) game server written in [Sesterl](https://github.com/gfngfn/Sesterl), Erlang, and Elm.

The following is a capture video of playing the game:

- https://twitter.com/bd_gfngfn/status/1404112257517195268?s=20


## Memos for development

### Build dependencies

* make
* Sass
* Erlang/OTP
* Rebar3
* Elm
* [APBuf](https://github.com/gfngfn/apbuf)
  - Used for generating a JSON encoder/decoder from `model.apbuf`.
  - Can be installed via OPAM pin:
    ```console
    $ git clone git@github.com:gfngfn/apbuf
    $ cd apbuf
    $ opam pin add apbuf .
    ```
* [Sesterl](https://github.com/gfngfn/Sesterl)
  - A statically-typed Erlang.
  - Can be installed via OPAM pin.
    ```console
    $ git clone git@github.com:gfngfn/Sesterl
    $ cd Sesterl
    $ opam pin add sesterl .
    ```


### How to build

Just invoke `make`.


### How to run tests

Invoke `make test`.


### How to launch locally

Invoke `make run`.


### Sequence Diagrams

See `model.apbuf` for the definition of the formats of request/response bodies.


#### Login

```
Client                             Server
  | POST /users                      |
  | <create_user_request>            |
  |--------------------------------->|
  |                                  |
  |       201 <create_user_response> |
  |<---------------------------------|
  |                                  |
  | /websocket/users/<user_id>       |
  |--------------------------------->|
  |                                  |
  |                              100 |
  |<---------------------------------|
  |                                  |
  | GET /rooms                       |
  |--------------------------------->|
  |                                  |
  |     200 <get_all_rooms_response> |
  |<---------------------------------|
```


#### Entering a room

```
Client                                            Server
  |                                                  |
  | PATCH /rooms                                     |
  | RoomRequestToEnterRoom(<enter_room_request>)     |
  |------------------------------------------------->|
  |                                                  |
  |                        200 <enter_room_response> |
  |<-------------------------------------------------|
```


#### Card submission

```
Client 1                                              Server                            Client 2, 3, and 4
  |                                                      |                                   |    |    |
  | PATCH /rooms                                         |                                   |    |    |
  | RoomRequestToSubmitCards(<submit_cards_request>)     |                                   |    |    |
  |----------------------------------------------------->|                                   |    |    |
  |                                                      | NotifySubmission(<submission>)    |    |    |
  |                                                      |---------------------------------->|    |    |
  |                                                      |--------------------------------------->|    |
  |                          200 <submit_cards_response> |-------------------------------------------->|
  |<-----------------------------------------------------|                                   |    |    |
  |                                                      |                                   |    |    |
  |                                                      |         CommandAck(<snapshot_id>) |    |    |
  |                                                      |<----------------------------------|    |    |
  |                                                      |         CommandAck(<snapshot_id>) |    |    |
  | CommandAck(<snapshot_id>)                            |<--------------------------------------------|
  |----------------------------------------------------->|                                   |    |    |
  |                                                      |         CommandAck(<snapshot_id>) |    |    |
  |                                       NotifyNextStep |<---------------------------------------|    |
  |<-----------------------------------------------------| NotifyNextStep                    |    |    |
  |                                                      |---------------------------------->|    |    |
  |                                                      |--------------------------------------->|    |
  |                                                      |-------------------------------------------->|
  |                                                      |                                   |    |    |
```


### How to deploy the game server on an EC2 instance

1. Make branch `temp-deploy` which contains the build targets generated by APBuf, Elm, and Sesterl.

   - This step shall be removed in the future; the pipeline scripts for building programs written in APBuf, Elm, and Sesterl will be needed.

2. Generate your key pair (the name of which is written as `〈NameOfYourKey〉` hereafter).

3. Create an IAM role `CloudWatchAgentServerRole` to which `CloudWatchAgentServerPolicy` is assigned.

4. Invoke the following command to deploy on an EC2 instance (this step takes approximately 10 minutes):

   ```console
   $ cd 〈RootOfThisRepository〉
   $ aws cloudformation deploy --stack-name "〈NameOfYourStack〉" --template-file aws/cloud-formation.template.yaml --parameter-overrides "KeyName=〈NameOfYourKey〉"
   ```

   - Then you can access the instance by using SSH like the following:

     ```console
     $ ssh -i ~/.ssh/〈NameOfYourKey〉.pem ubuntu@〈AssignedIPAddress〉
     ```

5. Get the IP address `〈AssignedIPAddress〉` assigned to the instance and visit the address by using a browser.


### Acknowledgement

* The cloudFormation template in the following repository has been largely used as a reference:
  - [thiagoesteves/erlgame: Snake Game webserver written in Erlang using cowboy as webserver. there is an aws cloud formation to deploy the game at aws amazon](https://github.com/thiagoesteves/erlgame)
