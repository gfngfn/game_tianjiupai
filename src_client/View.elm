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
        AtEntrance userNameInput ->
          viewEntrance userNameInput

        AtPlaza user roomNameInput maybeRooms ->
          viewPlaza user roomNameInput maybeRooms

        InRoom user room chatTextInput roomState ->
          viewRoom user room chatTextInput roomState
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


viewPlaza : User -> RoomName -> Maybe (List Room) -> Html Msg
viewPlaza user roomNameInput maybeRooms =
  div []
    [ div []
        [ text ("Hi, " ++ user.name ++ "! (your user ID: " ++ user.id ++ ")") ]
    , viewRoomList maybeRooms
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


viewRoomList : Maybe (List Room) -> Html Msg
viewRoomList maybeRooms =
  case maybeRooms of
    Nothing ->
      div [] [ text "(Rooms will be displayed here)" ]

    Just rooms ->
      let
        elems =
          rooms |> List.map (\room ->
            let members = String.join ", " room.members in
            li []
            [ text (room.name ++ " (room ID: " ++ room.id ++ ", members: " ++ members ++ ")")
            , button
                [ onClick (SendRequest (EnterRoom room.id)) ]
                [ text "enter" ]
            ]
          )
      in
      ul [] elems


viewRoom : User -> Room -> String -> RoomState -> Html Msg
viewRoom user room chatTextInput roomState =
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
                , value chatTextInput
                , onInput (UpdateInput << ChatInput)
                ] []
            , button
                [ onClick (SendRequest SendChat) ]
                [ text "send" ]
            ]
        ]
    ]
