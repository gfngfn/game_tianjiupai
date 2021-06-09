module View exposing (viewBody)

import Set exposing (Set)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Models exposing (..)
import Common exposing (..)
import Game


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
          let turn = Game.isMyTurn user.userId ostate in
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
            , div []
                [ text ("your turn: " ++ (if turn then "Y" else "N")) ]
            , viewObservableInning user.userId indices ostate.observableInning
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


viewObservableInning : UserId -> Set Int -> ObservableInning -> Html Msg
viewObservableInning userId indices observableInning =
  case observableInning of
    ObservableDuringInning oinning ->
      let
        gainsQuad = oinning.gains
        startsAt  = oinning.startsAt
        table     = oinning.table
        yourHand  = oinning.yourHand
      in
      div []
        [ div [] [ text "ObservableDuringInning" ]
        , showGainsQuad gainsQuad
        , showTable table
        , showHand indices yourHand
        ]

    ObservableInningEnd gainsQuad ->
      div []
        [ div [] [ text "ObservableInningEnd" ]
        , showGainsQuad gainsQuad
        ]


showTable : Table -> Html Msg
showTable table =
  let
    msg =
      case table of
        Starting          -> "Starting"
        TrickWuzun e      -> "TrickWuzun, " ++ showExposed (\_ -> "wuzun") e
        TrickWenzun e     -> "TrickWenzun, " ++ showExposed (\b -> if b then "y" else "n") e
        TrickSingleWen e  -> "TrickSingleWen, " ++ showExposed showWen e
        TrickSingleWu e   -> "TrickSingleWu, " ++ showExposed showWu e
        TrickDoubleWen e  -> "TrickDoubleWen, " ++ showExposed showWen e
        TrickDoubleWu e   -> "TrickDoubleWu, " ++ showExposed showWu e
        TrickDoubleBoth e -> "TrickDoubleBoth, " ++ showExposed showBig e
        TrickTripleWen e  -> "TrickTripleWen, " ++ showExposed showBig e
        TrickTripleWu e   -> "TrickTripleWu, " ++ showExposed showBig e
        TrickQuadruple e  -> "TrickQuadruple, " ++ showExposed showBig e
  in
  div []
    [ text msg ]


showExposed : (a -> String) -> Exposed a -> String
showExposed pf exposed =
  let
    s0 =
      pf exposed.first

    ss =
      exposed.subsequent |> List.map (\xOrClosed ->
        case xOrClosed of
          Open x -> pf x
          Closed -> "close"
      )
  in
  (s0 :: ss) |> String.join "-"


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


showHand : Set Int -> List Card -> Html Msg
showHand indices cards =
  let
    elems =
      cards |> List.indexedMap (\index card ->
        if indices |> Set.member index then
          li [] [ text ("@ " ++ showCard card) ]
        else
          li [] [ text ("- " ++ showCard card) ]
      )
  in
  div []
    [ div []
        [ text "hands:" ]
    , ol []
        elems
    ]


showCards : List Card -> String
showCards cards =
  cards |> List.map showCard |> String.join ", "


showCard : Card -> String
showCard card =
  case card of
    Wen wen -> showWen wen
    Wu wu   -> showWu wu


showBig : CardBig -> String
showBig big =
  "big" ++ String.fromInt big


showWen : CardWen -> String
showWen wen =
  "wen" ++ String.fromInt wen


showWu : CardWu -> String
showWu wu =
  "wu" ++ String.fromInt wu
