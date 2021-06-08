module View exposing (viewBody)

import Set exposing (Set)
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

        InRoom _ user personalState indices chatTextInput ->
          viewRoom user personalState indices chatTextInput
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


viewRoom : User -> PersonalState -> Set Int -> String -> Html Msg
viewRoom user pstate indices chatTextInput =
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

        PlayingGame ostate ->
          let gameMeta = ostate.meta in
          div []
            [ div []
                [ text (room.roomName ++ " (room ID: " ++ room.roomId ++ ")") ]
            , div []
                [ text ("inning index: " ++ String.fromInt gameMeta.inningIndex) ]
            , div []
                [ text ("number of consecutives: " ++ String.fromInt gameMeta.numConsecutives) ]
            , viewPlayers gameMeta.players
            , div []
                [ text ("snapshot ID: " ++ ostate.snapshotId) ]
            , div []
                [ text ("synchronizing: " ++ (if ostate.synchronizing then "Y" else "N")) ]
            , viewObservableInning user.userId ostate.observableInning
            ]
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


viewPlayers : PerSeat GamePlayer -> Html Msg
viewPlayers players =
  div []
    [ div []
        [ text "players:" ]
    , ol []
        [ li [] [ text ("seat 0: " ++ players.east.userId  ++ ", score: " ++ String.fromInt players.east.score) ]
        , li [] [ text ("seat 1: " ++ players.south.userId ++ ", score: " ++ String.fromInt players.south.score) ]
        , li [] [ text ("seat 2: " ++ players.west.userId  ++ ", score: " ++ String.fromInt players.west.score) ]
        , li [] [ text ("seat 3: " ++ players.north.userId ++ ", score: " ++ String.fromInt players.north.score) ]
        ]
    ]


viewObservableInning : UserId -> ObservableInning -> Html Msg
viewObservableInning userId observableInning =
  case observableInning of
    ObservableDuringInning oistate ->
      let
        gainsQuad = oistate.gains
        startsAt  = oistate.startsAt
        table     = oistate.table
        yourHand  = oistate.yourHand
      in
      div []
        [ div [] [ text "ObservableDuringInning" ]
        , showGainsQuad gainsQuad
        ]

    ObservableInningEnd gainsQuad ->
      div []
        [ div [] [ text "ObservableInningEnd" ]
        , showGainsQuad gainsQuad
        ]
        -- TODO


showGainsQuad : PerSeat (List Card) -> Html Msg
showGainsQuad gainsQuad =
  div []
    [ div []
        [ text "gains:" ]
    , ol []
        [ li [] [ text ("seat 0: " ++ showCards gainsQuad.east) ]
        , li [] [ text ("seat 1: " ++ showCards gainsQuad.south) ]
        , li [] [ text ("seat 2: " ++ showCards gainsQuad.west) ]
        , li [] [ text ("seat 3: " ++ showCards gainsQuad.north) ]
        ]
    ]


showCards : List Card -> String
showCards cards =
  cards |> List.map showCard |> String.join ", "


showCard : Card -> String
showCard card =
  case card of
    Wen wen -> "wen" ++ String.fromInt wen
    Wu wu   -> "wu" ++ String.fromInt wu
