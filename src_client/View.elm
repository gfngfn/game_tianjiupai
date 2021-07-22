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
    { header = [ text "天九 Online" ]
    , middle = middle
    , style  = "entrance-middle"
    , footer = message
    }


viewPlaza : ( MessageLevel, String ) -> User -> RoomName -> Maybe (List RoomSummary) -> List (Html Msg)
viewPlaza message user roomNameInput maybeRoomSummaries =
  let
    middle =
      [ div [ class "plaza-container" ]
          ([ h1 []
              [ text "部屋一覧" ]
          , div [ class "room-creation-input-group" ]
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
    { header = [ text ("天九 Online | " ++ user.userName ++ " さん"), specialButton True "ログアウト" DeleteUser ]
    , middle = middle
    , style  = "plaza-middle"
    , footer = message
    }


viewRoomList : Maybe (List RoomSummary) -> List (Html Msg)
viewRoomList maybeRoomSummaries =
  case maybeRoomSummaries of
    Nothing ->
      [ div [] [ text "部屋一覧取得中……" ] ]

    Just [] ->
      [ div [] [ text "まだ部屋がありません．右上の入力部分から部屋を作成できます" ] ]

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
              [ div [ class "room-name" ]
                  [ text room.roomName ]
              , div []
                  [ text ("参加者: " ++ members) ]
              ]
          , div [ class "plaza-panel-right" ]
              [ div [ class "status-text" ]
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

    headerText : String
    headerText = "天九 Online | " ++ user.userName ++ " さん"
  in
  let
    elemsChat : List (Html Msg)
    elemsChat =
      [ div [ class "log-area" ]
          (pstate.logs |> List.map viewLogEntry)
      , div [ class "chat-input-container" ]
          [ specialInput
              { placeholder = "コメント"
              , value       = chatTextInput
              , update      = ChatInput
              }
          , specialButton (not (String.isEmpty chatTextInput)) "送信" SendChat
          ]
      ]
  in
  case pstate.game of
    WaitingStart users ->
      let
        elemsDebug =
          let members = String.join ", " (users |> List.map (\u -> u.userName)) in
          [ div []
              [ text "debug info" ]
          , ul []
              [ li []
                  [ text ("room ID: " ++ room.roomId) ]
              , li []
                  [ text ("members: " ++ members) ]
              ]
          ]

        elemsLeft =
          [ div [ class "room-name" ]
              [ text room.roomName ]
          , div []
              [ text "待機中" ]
          , div []
              [ specialButton True "退室" (ExitRoom room.roomId) ]
          , div [ class "debug-info" ]
              elemsDebug
          ]
      in
      viewRoomGridScheme
        { header = [ text headerText ]
        , left   = elemsLeft
        , center = []
        , right  = elemsChat
        , footer = message
        }

    PlayingGame ostate ->
      let gameMeta = ostate.meta in
      let scores = gameMeta.scores in
      let players = gameMeta.players in
      let synchronizing = ostate.synchronizing in
      let turn = Game.isMyTurn user.userId ostate in
      let userId = user.userId in
      case
        players |> PerSeat.find (\maybePlayer ->
          case maybePlayer of
            Just player -> player.user.userId == userId
            Nothing     -> False
        )
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

            handInfo : HandInfo
            handInfo =
              { maybeIndices  = if turn then Just indices else Nothing
              , maybeTable    = maybeTable
              , synchronizing = synchronizing
              }

            elemsDebug : List (Html Msg)
            elemsDebug =
              [ div [] [ text "debug info" ]
              , ul []
                  [ li [] [ text ("room ID: " ++ room.roomId) ]
                  , li [] [ text ("snapshot ID: " ++ ostate.snapshotId) ]
                  , li [] [ text ("synchronizing: " ++ (if synchronizing then "Y" else "N")) ]
                  , li [] [ text ("your turn: " ++ (if turn then "Y" else "N")) ]
                  ]
              ]

            elemsLeft : List (Html Msg)
            elemsLeft =
              [ div [ class "room-name" ]
                  [ text room.roomName ]
              , div []
                  [ text (showGameIndex gameMeta.inningIndex gameMeta.numConsecutives) ]
              , viewPlayer "東" players.east  scores.east
              , viewPlayer "南" players.south scores.south
              , viewPlayer "西" players.west  scores.west
              , viewPlayer "北" players.north scores.north
              , div [ class "debug-info" ]
                  elemsDebug
              ]
          in
          viewRoomGridScheme
            { header = [ text headerText ]
            , left   = elemsLeft
            , center = [ ViewTable.view userId seat gameMeta.parentSeat handInfo ostate.observableInning ]
            , right  = elemsChat
            , footer = message
            }


viewLogEntry : Log -> Html Msg
viewLogEntry log =
  case log of
    LogComment comment ->
      div [ class "log-entry" ] [ b [] [ text comment.from.userName ], text (": " ++ comment.text) ]

    LogEntered u ->
      div [ class "log-entry" ] [ b [] [ text u.userName ], text " さんが参加しました" ]

    LogExited u ->
      div [ class "log-entry" ] [ b [] [ text u.userName ], text " さんが退出しました" ]

    LogGameStart gameIndex ->
      let s = showGameIndex gameIndex.inningIndex gameIndex.numConsecutives in
      div [ class "log-entry" ] [ b [] [ text (s ++ " 開始！") ] ]

    LogDiffs diffs ->
      let
        diffText =
          String.join ", "
            (List.map String.fromInt [ diffs.east, diffs.south, diffs.west, diffs.north ])
      in
      div [ class "log-entry" ] [ text diffText ]

    LogConnection connection ->
      let u = connection.user in
      let
        suffix =
          if connection.isConnected then
            " さんが再接続しました"
          else
            " さんの接続が切れました"
      in
      div [ class "log-entry" ] [ b [] [ text u.userName ], text suffix ]


showGameIndex : Int -> Int -> String
showGameIndex inningIndex numConsecutives =
  case showInningIndex inningIndex of
    Just s  -> s ++ "・" ++ (String.fromInt (numConsecutives + 1)) ++ "倍場"
    Nothing -> "終局"


showInningIndex : Int -> Maybe String
showInningIndex inningIndex =
  if inningIndex < 0 then
    Nothing
  else if inningIndex < 4 then
    Just ("東" ++ (String.fromInt (inningIndex + 1)) ++ "局")
  else if inningIndex < 8 then
    Just ("南" ++ (String.fromInt (inningIndex - 3)) ++ "局")
  else
    Nothing


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


viewPlayer : String -> Maybe GamePlayer -> Int -> Html Msg
viewPlayer direction maybePlayer score =
  let
    userName =
      case maybePlayer of
        Just player -> player.user.userName
        Nothing     -> "空席"
  in
  div [ class "panel" ]
    [ div [ class "player-name" ] [ text (direction ++ " " ++ userName) ]
    , div [] [ text ("得点： " ++ String.fromInt score) ]
    ]
