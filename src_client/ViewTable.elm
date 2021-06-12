module ViewTable exposing (view, HandInfo)

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


view : UserId -> HandInfo -> ObservableInning -> Html Msg
view userId handInfo observableInning =
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
