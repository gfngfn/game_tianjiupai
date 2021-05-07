module View exposing (viewBody)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Common exposing (..)


viewBody : Model -> List (Html Msg)
viewBody model =
  let
    elemMain =
      case model.user of
        Nothing ->
          viewEntrance model

        Just user ->
          case user.belongsTo of
            Nothing ->
              viewRoomListPage model user

            Just roomId ->
              viewRoomPage model roomId
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


viewRoomListPage : Model -> User -> Html Msg
viewRoomListPage model user =
  div []
    [ div []
        [ text ("Hi, " ++ user.name ++ "! (your user ID: " ++ user.id ++ ")") ]
    , viewRoomList model
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


viewRoomList : Model -> Html Msg
viewRoomList model =
  case model.rooms of
    Nothing ->
      div [] [ text "(Rooms will be shown here)" ]

    Just rooms ->
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


viewRoomPage : Model -> RoomId -> Html Msg
viewRoomPage model roomId =
  let
    members =
      case model.rooms of
        Nothing ->
          "(Cannot find the room. Please reload the page)"

        Just rooms ->
          case List.filter (\room -> room.id == roomId) rooms of
            []        -> "(Cannot find the room. Please reload the page)"
            room :: _ -> String.join ", " room.members
  in
  div []
    [ div []
        [ div []
            [ text ("Room (ID: " ++ roomId ++ ", members: " ++ members ++ ")") ]
        ]
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
