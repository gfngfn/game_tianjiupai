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


specialButton : Bool -> String -> Request -> Html Msg
specialButton enabled buttonText req =
  if enabled then
    span [ class "enabled-button", onClick (SendRequest req) ]
      [ text buttonText ]
  else
    span [ class "disabled-button" ]
      [ text buttonText ]


type alias InputData =
  { value       : String
  , placeholder : String
  , update      : String -> InputUpdate
  }


specialInput : InputData -> Html Msg
specialInput data =
  span [ class "input-container" ]
    [ input
        [ type_ "text"
        , class "input-main"
        , placeholder data.placeholder
        , value data.value
        , onInput (UpdateInput << data.update)
        ]
        []
    ]


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
viewEntrance message userNameInput =
  let
    middle =
      [ div [ class "entrance-container" ]
          [ div [ class "entrance-input-group" ]
              [ specialInput
                  { placeholder = "ユーザ名"
                  , value       = userNameInput
                  , update      = UserNameInput
                  }
              , specialButton (not (String.isEmpty userNameInput)) "開始" CreateUser
              ]
          , div [ class "entrance-explanation" ]
              [ ul []
                  [ li []
                      [ text "アカウント登録なしで遊べます．"
                      , ul []
                          [ li [] [ text "ユーザはcookieによって識別されます．" ]
                          , li [] [ text "ユーザ名はユーザを一意に識別するIDではなく表示上の文字列です．" ]
                          ]
                      ]
                  , li []
                      [ text "機能上は対戦データ等を永続化して記録することは特にありません．" ]
                  , li []
                      [ text "現時点ではセキュリティ対策が万全とは限りませんのでご了承ください．" ]
                  ]
              ]
          ]
      ]
  in
  viewSimpleGridScheme
    { header = [ text "header" ]
    , middle = middle
    , style  = "entrance-middle"
    , footer = message
    }


viewPlaza : ( MessageLevel, String ) -> User -> RoomName -> Maybe (List RoomSummary) -> List (Html Msg)
viewPlaza message user roomNameInput maybeRoomSummaries =
  let
    middle =
      [ div [ class "plaza-container" ]
          ([ div []
              [ text ("ようこそ，" ++ user.userName ++ " さん (ユーザID: " ++ user.userId ++ ")") ]
          , div []
              [ specialInput
                  { placeholder = "部屋名"
                  , value       = roomNameInput
                  , update      = RoomNameInput
                  }
              , specialButton (not (String.isEmpty roomNameInput)) "作成" CreateRoom
              ]
          ] ++ viewRoomList maybeRoomSummaries)
      ]
  in
  viewSimpleGridScheme
    { header = [ text "header" ]
    , middle = middle
    , style  = "plaza-middle"
    , footer = message
    }


viewRoomList : Maybe (List RoomSummary) -> List (Html Msg)
viewRoomList maybeRoomSummaries =
  case maybeRoomSummaries of
    Nothing ->
      [ div [] [ text "部屋一覧取得中……" ] ]

    Just roomSummaries ->
      roomSummaries |> List.map (\roomSummary ->
        let
          room = roomSummary.room
          members = String.join ", " (roomSummary.members |> List.map (\u -> u.userName))

          statusText =
            if roomSummary.isPlaying then
              "対局中"
            else
              "待機中"
        in
        div [ class "plaza-panel" ]
          [ div [ class "plaza-panel-left" ]
              [ div []
                  [ text (room.roomName ++ " (部屋ID: " ++ room.roomId ++ ")") ]
              , div []
                  [ text ("参加者: " ++ members) ]
              ]
          , div [ class "plaza-panel-right" ]
              [ div []
                  [ text statusText ]
              , div []
                  [ specialButton True "参加" (EnterRoom room.roomId) ]
              ]
          ]
      )


viewRoom : ( MessageLevel, String ) -> User -> PersonalState -> Set Int -> String -> List (Html Msg)
viewRoom message user pstate indices chatTextInput =
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
  case pstate.game of
    WaitingStart users ->
      let members = String.join ", " (users |> List.map (\u -> u.userName)) in
      let
        elemsDebug =
          [ div []
              [ text (room.roomName ++ " (部屋ID: " ++ room.roomId ++ ", 参加者: " ++ members ++ ")") ]
          ]
      in
      viewRoomGridScheme
        { header = [ text "header" ]
        , left   = elemsDebug
        , center = []
        , right  = elemsChat
        , footer = message
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
          viewRoomGridScheme
            { header = [ text "header" ]
            , left   = elemsLeft
            , center = [ ViewTable.view userId seat handInfo ostate.observableInning ]
            , right  = elemsChat
            , footer = message
            }


type alias SimpleGridScheme =
  { header : List (Html Msg)
  , middle : List (Html Msg)
  , style  : String
  , footer : ( MessageLevel, String )
  }


viewSimpleGridScheme : SimpleGridScheme -> List (Html Msg)
viewSimpleGridScheme gridScheme =
  let ( level, messageText ) = gridScheme.footer in
  [ div [ class "simple-grid-container" ]
      [ div [ class "simple-grid-element-header" ] gridScheme.header
      , div [ class "simple-grid-element-middle", class gridScheme.style ] gridScheme.middle
      , div [ class "simple-grid-element-footer", class (footerStyleFromLevel level) ] [ text messageText ]
      ]
  ]


type alias RoomGridScheme =
  { header : List (Html Msg)
  , left   : List (Html Msg)
  , center : List (Html Msg)
  , right  : List (Html Msg)
  , footer : ( MessageLevel, String )
  }


viewRoomGridScheme : RoomGridScheme -> List (Html Msg)
viewRoomGridScheme gridScheme =
  let ( level, messageText ) = gridScheme.footer in
  [ div [ class "room-grid-container" ]
      [ div [ class "room-grid-element-header" ] gridScheme.header
      , div [ class "room-grid-element-left" ]   gridScheme.left
      , div [ class "room-grid-element-center" ] gridScheme.center
      , div [ class "room-grid-element-right" ]  gridScheme.right
      , div [ class "room-grid-element-footer", class (footerStyleFromLevel level) ] [ text messageText ]
      ]
  ]


footerStyleFromLevel : MessageLevel -> String
footerStyleFromLevel level =
  case level of
    Information -> "footer-style-normal"
    Warning     -> "footer-style-warning"


viewPlayer : String -> GamePlayer -> Html Msg
viewPlayer direction player =
  div [ class "panel" ]
    [ div [ class "player-name" ] [ text (direction ++ " " ++ player.user.userName) ]
    , div [] [ text ("得点： " ++ String.fromInt player.score) ]
    ]
