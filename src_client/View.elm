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


enabledButton : String -> Request -> Html Msg
enabledButton buttonText req =
  span [ class "enabled-button", onClick (SendRequest req) ]
    [ text buttonText ]


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

    middle =
      [ div [ class "entrance-container" ]
          [ div []
              [
                input
                [ type_ "text"
                , placeholder "ユーザ名"
                , value userNameInput
                , onInput (UpdateInput << UserNameInput)
                ] []
              , enabledButton "開始" CreateUser
              ]
          , div [] [ text "※ユーザはcookieによって識別されるため，アカウント登録等は不要です．" ]
          ]
      ]
  in
  viewSimpleGridScheme
    { header = [ text "header" ]
    , middle = middle
    , footer = viewFooter level message
    }


viewPlaza : ( MessageLevel, String ) -> User -> RoomName -> Maybe (List RoomSummary) -> List (Html Msg)
viewPlaza ( level, message ) user roomNameInput maybeRoomSummaries =
  let
    sty =
      case level of
        Information -> style "color" "gray"
        Warning     -> style "color" "red"
  in
  [ div [ class "plaza-container" ]
      ([ div []
          [ div [ sty ] [ text message ] ]
      , div []
          [ text ("ようこそ，" ++ user.userName ++ " さん (ユーザID: " ++ user.userId ++ ")") ]
      , div []
          [ input
              [ type_ "text"
              , placeholder "部屋名"
              , value roomNameInput
              , onInput (UpdateInput << RoomNameInput)
              ] []
          , enabledButton "作成" CreateRoom
          ]
      ] ++ viewRoomList maybeRoomSummaries)
  ]


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
                  [ enabledButton "参加" (EnterRoom room.roomId) ]
              ]
          ]
      )


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
        , footer = viewFooter level message
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
            , footer = viewFooter level message
            }


type alias SimpleGridScheme =
  { header : List (Html Msg)
  , middle : List (Html Msg)
  , footer : List (Html Msg)
  }


viewSimpleGridScheme : SimpleGridScheme -> List (Html Msg)
viewSimpleGridScheme gridScheme =
  [ div [ class "simple-grid-container" ]
      [ div [ class "simple-grid-element-header" ] gridScheme.header
      , div [ class "simple-grid-element-middle" ] gridScheme.middle
      , div [ class "simple-grid-element-footer" ] gridScheme.footer
      ]
  ]


type alias RoomGridScheme =
  { header : List (Html Msg)
  , left   : List (Html Msg)
  , center : List (Html Msg)
  , right  : List (Html Msg)
  , footer : List (Html Msg)
  }


viewRoomGridScheme : RoomGridScheme -> List (Html Msg)
viewRoomGridScheme gridScheme =
  [ div [ class "room-grid-container" ]
      [ div [ class "room-grid-element-header" ] gridScheme.header
      , div [ class "room-grid-element-left" ]   gridScheme.left
      , div [ class "room-grid-element-center" ] gridScheme.center
      , div [ class "room-grid-element-right" ]  gridScheme.right
      , div [ class "room-grid-element-footer" ] gridScheme.footer
      ]
  ]


viewFooter : MessageLevel -> String -> List (Html Msg)
viewFooter level message =
  let
    stys =
      case level of
        Information -> [ class "footer-style-normal" ]
        Warning     -> [ class "footer-style-warning" ]
  in
  [ div stys [ text message ] ]


viewPlayer : String -> GamePlayer -> Html Msg
viewPlayer direction player =
  div [ class "panel" ]
    [ div [ class "player-name" ] [ text (direction ++ " " ++ player.user.userName) ]
    , div [] [ text ("得点： " ++ String.fromInt player.score) ]
    ]
