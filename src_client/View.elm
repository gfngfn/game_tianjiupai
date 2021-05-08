module View exposing (viewBody)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Common exposing (..)


viewBody : Model -> List (Html Msg)
viewBody model =
  let
    elemMain =
      case model.state of
        AtEntrance ->
          viewEntrance model

        AtPlaza r ->
          viewPlaza model r.user r.rooms

        InRoom r ->
          viewRoom model r.user r.room r.roomState
  in
  [ div []
      [ div [] [ text model.message ]
      , elemMain
      ]
  ]


viewEntrance : Model -> Html Msg
viewEntrance model =
  div []
    [ input
        [ type_ "text"
        , placeholder "username"
        , value model.inputs.userName
        , onInput (UpdateInput << UserNameInput)
        ] []
    , button
        [ onClick (Send CreateUser) ]
        [ text "start" ]
    ]


viewPlaza : Model -> User -> List Room -> Html Msg
viewPlaza model user rooms =
  div []
    [ div []
        [ text ("Hi, " ++ user.name ++ "! (your user ID: " ++ user.id ++ ")") ]
    , viewRoomList rooms
    , div []
        [ input
            [ type_ "text"
            , placeholder "new room name"
            , value model.inputs.roomName
            , onInput (UpdateInput << RoomNameInput)
            ] []
        , button
            [ onClick (Send CreateRoom) ]
            [ text "create" ]
        ]
    ]


viewRoomList : List Room -> Html Msg
viewRoomList rooms =
  let
    elems =
      List.map (\room ->
        let members = String.join ", " room.members in
        li []
        [ text (room.name ++ " (room ID: " ++ room.id ++ ", members: " ++ members ++ ")")
        , button
            [ onClick (Send (EnterRoom room.id)) ]
            [ text "enter" ]
        ]
      ) rooms
  in
  ul [] elems


viewRoom : Model -> User -> Room -> RoomState -> Html Msg
viewRoom model user room roomState =
  let
    members =
      String.join ", " room.members
  in
  div []
    [ div []
        [ div []
            [ text (room.name ++ " (room ID: " ++ room.id ++ ", members: " ++ members ++ ")") ]
        ]
    , ul []
        (room.logs |> List.map (\log ->
          case log of
            LogComment from s ->
              li [] [ b [] [ text from ], text (": " ++ s) ]

            LogEntered userId ->
              li [] [ b [] [ text userId ], text " entered." ]

            LogExited userId ->
              li [] [ b [] [ text userId ], text " exited." ]
        ))
    , div []
        [ div []
            [ input
                [ type_ "text"
                , placeholder "comment"
                , value model.inputs.chatText
                , onInput (UpdateInput << ChatInput)
                ] []
            , button
                [ onClick (SendWebSocketMessage SendChat) ]
                [ text "send" ]
            ]
        ]
    ]
