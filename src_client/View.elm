module View exposing (viewBody)

import Set exposing (Set)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Models exposing (..)
import Common exposing (..)
import Game
import PerSeat
import ViewTable exposing (HandInfo)


viewBody : Model -> List (Html Msg)
viewBody model =
  let message = model.message in
  case model.state of
    AtEntrance userNameInput _ ->
      viewEntrance message userNameInput

    AtPlaza _ user roomNameInput maybeRoomSummaries ->
      viewPlaza message user roomNameInput maybeRoomSummaries

    InRoom _ user personalState indices chatTextInput ->
      viewRoom message user personalState indices chatTextInput


viewEntrance : ( MessageLevel, String ) -> UserName -> List (Html Msg)
viewEntrance ( level, message ) userNameInput =
  let
    sty =
      case level of
        Information -> style "color" "gray"
        Warning     -> style "color" "red"
  in
  [ div []
      [ div [ sty ] [ text message ] ]
  , input
      [ type_ "text"
      , placeholder "ユーザ名"
      , value userNameInput
      , onInput (UpdateInput << UserNameInput)
      ] []
  , button
      [ onClick (SendRequest CreateUser) ]
      [ text "開始" ]
  ]


viewPlaza : ( MessageLevel, String ) -> User -> RoomName -> Maybe (List RoomSummary) -> List (Html Msg)
viewPlaza ( level, message ) user roomNameInput maybeRoomSummaries =
  let
    sty =
      case level of
        Information -> style "color" "gray"
        Warning     -> style "color" "red"
  in
  [ div []
      [ div [ sty ] [ text message ] ]
  , div []
      [ text ("ようこそ，" ++ user.userName ++ " さん (ユーザID: " ++ user.userId ++ ")") ]
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


viewRoom : ( MessageLevel, String ) -> User -> PersonalState -> Set Int -> String -> List (Html Msg)
viewRoom ( level, message ) user pstate indices chatTextInput =
  let
    room : Room
    room = pstate.room
  in
  let
    elemsChat : List (Html Msg)
    elemsChat =
      [ ul []
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
  in
  let
    stys =
      case level of
        Information -> [ class "grid-element-footer", class "footer-style-normal" ]
        Warning     -> [ class "grid-element-footer", class "footer-style-warning" ]
  in
  case pstate.game of
    WaitingStart users ->
      let members = String.join ", " (users |> List.map (\u -> u.userName)) in
      let
        elemsDebug =
          [ div []
              [ text (room.roomName ++ " (部屋ID: " ++ room.roomId ++ ", 参加者: " ++ members ++ ")") ]
          ]
      in
      viewGridScheme
        { header = [ text "header" ]
        , left   = elemsDebug
        , center = []
        , right  = elemsChat
        , footer = [ div stys [ text message ] ]
        }

    PlayingGame ostate ->
      let gameMeta = ostate.meta in
      let players = gameMeta.players in
      let synchronizing = ostate.synchronizing in
      let turn = Game.isMyTurn user.userId ostate in
      let userId = user.userId in
      case
        players |> PerSeat.find (\p -> p.user.userId == userId)
      of
        Nothing ->
        -- Should never happen
          [ div [] [ text "broken" ] ]

        Just seat ->
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
          let
            elemsDebug : List (Html Msg)
            elemsDebug =
              [ div []
                  [ text ("room ID: " ++ room.roomId) ]
              , div []
                  [ text ("snapshot ID: " ++ ostate.snapshotId) ]
              , div []
                  [ text ("synchronizing: " ++ (if synchronizing then "Y" else "N")) ]
              , div []
                  [ text ("your turn: " ++ (if turn then "Y" else "N")) ]
              ]
          in
          let
            elemsLeft =
              [ div [ class "room-name" ]
                  [ text room.roomName ]
              , div []
                  [ text
                      (String.fromInt (gameMeta.inningIndex + 1) ++ "局目，"
                        ++ String.fromInt (gameMeta.numConsecutives - 1) ++ "本場")
                  ]
              , viewPlayer "東" players.east
              , viewPlayer "南" players.south
              , viewPlayer "西" players.west
              , viewPlayer "北" players.north
              , div []
                  elemsDebug
              ]
          in
          viewGridScheme
            { header = [ text "header" ]
            , left   = elemsLeft
            , center = [ ViewTable.view userId seat handInfo ostate.observableInning ]
            , right  = elemsChat
            , footer = [ div stys [ text message ] ]
            }


type alias GridScheme =
  { header : List (Html Msg)
  , left   : List (Html Msg)
  , center : List (Html Msg)
  , right  : List (Html Msg)
  , footer : List (Html Msg)
  }


viewGridScheme : GridScheme -> List (Html Msg)
viewGridScheme gridScheme =
  [ div [ class "grid-container" ]
      [ div [ class "grid-element-header" ] gridScheme.header
      , div [ class "grid-element-left" ]   gridScheme.left
      , div [ class "grid-element-center" ] gridScheme.center
      , div [ class "grid-element-right" ]  gridScheme.right
      , div [ class "grid-element-footer" ] gridScheme.footer
      ]
  ]


viewPlayer : String -> GamePlayer -> Html Msg
viewPlayer direction player =
  div [ class "player-frame" ]
    [ div [ class "player-name" ] [ text (direction ++ " " ++ player.user.userName) ]
    , div [] [ text ("得点： " ++ String.fromInt player.score) ]
    ]
