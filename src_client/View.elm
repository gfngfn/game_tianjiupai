module View exposing (viewBody)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Models exposing (..)
import Common exposing (..)


viewBody : Model -> List (Html Msg)
viewBody model =
  let
    elemMain =
      case model.state of
        AtEntrance userNameInput _ ->
          viewEntrance userNameInput

        AtPlaza _ user roomNameInput maybeRoomSummaries ->
          viewPlaza user roomNameInput maybeRoomSummaries

        InRoom _ user personalState chatTextInput ->
          viewRoom user personalState chatTextInput
  in
  [ div []
      [ div [] [ text model.message ]
      , elemMain
      ]
  ]


viewEntrance : UserName -> Html Msg
viewEntrance userNameInput =
  div []
    [ input
        [ type_ "text"
        , placeholder "username"
        , value userNameInput
        , onInput (UpdateInput << UserNameInput)
        ] []
    , button
        [ onClick (SendRequest CreateUser) ]
        [ text "start" ]
    ]


viewPlaza : User -> RoomName -> Maybe (List RoomSummary) -> Html Msg
viewPlaza user roomNameInput maybeRoomSummaries =
  div []
    [ div []
        [ text ("Hi, " ++ user.userName ++ "! (your user ID: " ++ user.userId ++ ")") ]
    , viewRoomList maybeRoomSummaries
    , div []
        [ input
            [ type_ "text"
            , placeholder "new room name"
            , value roomNameInput
            , onInput (UpdateInput << RoomNameInput)
            ] []
        , button
            [ onClick (SendRequest CreateRoom) ]
            [ text "create" ]
        ]
    ]


viewRoomList : Maybe (List RoomSummary) -> Html Msg
viewRoomList maybeRoomSummaries =
  case maybeRoomSummaries of
    Nothing ->
      div [] [ text "(Rooms will be displayed here)" ]

    Just roomSummaries ->
      let
        elems =
          roomSummaries |> List.map (\roomSummary ->
            let
              room = roomSummary.room
              members = String.join ", " roomSummary.members
            in
            li []
            [ text (room.roomName ++ " (room ID: " ++ room.roomId ++ ", members: " ++ members ++ ")")
            , button
                [ onClick (SendRequest (EnterRoom room.roomId)) ]
                [ text "enter" ]
            ]
          )
      in
      ul [] elems


viewRoom : User -> PersonalState -> String -> Html Msg
viewRoom user pstate chatTextInput =
  let
    room : Room
    room = pstate.room

    elemHead : Html Msg
    elemHead =
      case pstate.game of
        WaitingStart userIds ->
          let members = String.join ", " userIds in
          div []
            [ div []
                [ text (room.roomName ++ " (room ID: " ++ room.roomId ++ ", members: " ++ members ++ ")") ]
            ]

        PlayingGame _ ->
          div [] [ text (room.roomName ++ " (room ID: " ++ room.roomId ++ ")") ]
  in
  div []
    [ elemHead
    , ul []
        (pstate.logs |> List.map (\log ->
          case log of
            LogComment comment ->
              li [] [ b [] [ text comment.from ], text (": " ++ comment.text) ]

            LogEntered userId ->
              li [] [ b [] [ text userId ], text " entered." ]

            LogExited userId ->
              li [] [ b [] [ text userId ], text " exited." ]

            LogGameStart ->
              li [] [ b [] [ text "Game start!" ] ]
        ))
    , div []
        [ div []
            [ input
                [ type_ "text"
                , placeholder "comment"
                , value chatTextInput
                , onInput (UpdateInput << ChatInput)
                ] []
            , button
                [ onClick (SendRequest SendChat) ]
                [ text "send" ]
            ]
        ]
    ]
