module View exposing (viewBody)

import Set exposing (Set)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Models exposing (..)
import Common exposing (..)
import Game


type alias HandInfo =
  { maybeIndices  : Maybe (Set Int)
  , maybeTable    : Maybe Table
  , synchronizing : Bool
  }


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
  let ( level, message ) = model.message in
  let
    sty =
      case level of
        Information -> style "color" "gray"
        Warning     -> style "color" "red"
  in
  [ div []
      [ div [ sty ] [ text message ]
      , elemMain
      ]
  ]


viewEntrance : UserName -> Html Msg
viewEntrance userNameInput =
  div []
    [ input
        [ type_ "text"
        , placeholder "ユーザ名"
        , value userNameInput
        , onInput (UpdateInput << UserNameInput)
        ] []
    , button
        [ onClick (SendRequest CreateUser) ]
        [ text "開始" ]
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
            , placeholder "部屋名"
            , value roomNameInput
            , onInput (UpdateInput << RoomNameInput)
            ] []
        , button
            [ onClick (SendRequest CreateRoom) ]
            [ text "作成" ]
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
              members = String.join ", " (roomSummary.members |> List.map (\u -> u.userName))
            in
            li []
            [ text (room.roomName ++ " (部屋ID: " ++ room.roomId ++ ", 参加者: " ++ members ++ ")")
            , button
                [ onClick (SendRequest (EnterRoom room.roomId)) ]
                [ text "参加" ]
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
        WaitingStart users ->
          let members = String.join ", " (users |> List.map (\u -> u.userName)) in
          div []
            [ div []
                [ text (room.roomName ++ " (部屋ID: " ++ room.roomId ++ ", 参加者: " ++ members ++ ")") ]
            ]

        PlayingGame ostate ->
          let gameMeta = ostate.meta in
          let synchronizing = ostate.synchronizing in
          let turn = Game.isMyTurn user.userId ostate in
          let
            maybeTable =
              case ostate.observableInning of
                ObservableDuringInning oinning -> Just oinning.table
                ObservableInningEnd _          -> Nothing
          in
          let
            handInfo : HandInfo
            handInfo =
              { maybeIndices  = if turn then Just indices else Nothing
              , maybeTable    = maybeTable
              , synchronizing = synchronizing
              }
          in
          div []
            [ div []
                [ text (room.roomName ++ " (部屋ID: " ++ room.roomId ++ ")") ]
            , div []
                [ text (String.fromInt gameMeta.inningIndex ++ "局目") ]
            , div []
                [ text (String.fromInt (gameMeta.numConsecutives - 1) ++ "本場") ]
            , viewPlayers gameMeta.players
            , div []
                [ text ("snapshot ID: " ++ ostate.snapshotId) ]
            , div []
                [ text ("synchronizing: " ++ (if synchronizing then "Y" else "N")) ]
            , div []
                [ text ("your turn: " ++ (if turn then "Y" else "N")) ]
            , viewObservableInning user.userId handInfo ostate.observableInning
            ]
  in
  div []
    [ elemHead
    , ul []
        (pstate.logs |> List.map (\log ->
          case log of
            LogComment comment ->
              li [] [ b [] [ text comment.from.userName ], text (": " ++ comment.text) ]

            LogEntered u ->
              li [] [ b [] [ text u.userName ], text " さんが参加しました" ]

            LogExited u ->
              li [] [ b [] [ text u.userName ], text " さんが退出しました" ]

            LogGameStart ->
              li [] [ b [] [ text "対局開始！" ] ]
        ))
    , div []
        [ div []
            [ input
                [ type_ "text"
                , placeholder "コメント"
                , value chatTextInput
                , onInput (UpdateInput << ChatInput)
                ] []
            , button
                [ onClick (SendRequest SendChat) ]
                [ text "送信" ]
            ]
        ]
    ]


viewPlayers : PerSeat GamePlayer -> Html Msg
viewPlayers players =
  div []
    [ div []
        [ text "players:" ]
    , ol []
        [ li [] [ text ("東 " ++ players.east.user.userName  ++ ", 得点: " ++ String.fromInt players.east.score) ]
        , li [] [ text ("南 " ++ players.south.user.userName ++ ", 得点: " ++ String.fromInt players.south.score) ]
        , li [] [ text ("西 " ++ players.west.user.userName  ++ ", 得点: " ++ String.fromInt players.west.score) ]
        , li [] [ text ("北 " ++ players.north.user.userName ++ ", 得点: " ++ String.fromInt players.north.score) ]
        ]
    ]


viewObservableInning : UserId -> HandInfo -> ObservableInning -> Html Msg
viewObservableInning userId handInfo observableInning =
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
        , showHand handInfo yourHand
        ]

    ObservableInningEnd gainsQuad ->
      div []
        [ div [] [ text "ObservableInningEnd" ]
        , showGainsQuad gainsQuad
        , button [ disabled handInfo.synchronizing, onClick (SendRequest RequireNextInning) ] [ text "次へ" ]
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
        [ text "山:" ]
    , ol []
        [ li [] [ text ("東 " ++ showCards gainsQuad.east) ]
        , li [] [ text ("南 " ++ showCards gainsQuad.south) ]
        , li [] [ text ("西 " ++ showCards gainsQuad.west) ]
        , li [] [ text ("北 " ++ showCards gainsQuad.north) ]
        ]
    ]


showHand : HandInfo -> List Card -> Html Msg
showHand handInfo cards =
  let
    elems =
      if handInfo.synchronizing then
        cards |> List.map (\card ->
          li [] [ text (showCard card) ]
        )
      else
        case handInfo.maybeIndices of
          Nothing ->
            cards |> List.map (\card ->
              li [] [ text ("| " ++ showCard card) ]
            )

          Just indices ->
            cards |> List.indexedMap (\index card ->
              if indices |> Set.member index then
                li []
                 [ div [ onClick (UnselectCard index) ]
                     [ text ("@ " ++ showCard card) ]
                 ]
              else
                li []
                 [ div [ onClick (SelectCard index) ]
                     [ text ("- " ++ showCard card) ]
                 ]
            )

    buttonElems =
      case handInfo.maybeIndices of
        Nothing ->
          []

        Just indices ->
          let selectedCards = getSelectedCards indices cards in
          let
            submittable =
              case handInfo.maybeTable of
                Nothing    -> False
                Just table -> Game.isSubmittable table selectedCards
          in
          [ button [ disabled (not submittable), onClick (SendRequest SubmitCards) ] [ text "submit" ] ]
  in
  div []
    ([ div [] [ text "手牌:" ], ol [] elems ] ++ buttonElems)


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
