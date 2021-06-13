module ViewTable exposing (view, HandInfo)

import Set exposing (Set)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE

import Models exposing (..)
import Common exposing (..)
import Game
import Constants as C


type alias HandInfo =
  { maybeIndices  : Maybe (Set Int)
  , maybeTable    : Maybe Table
  , synchronizing : Bool
  }

type alias RelativeElement =
  { gains     : List Card
  , submitted : List (ClosedOr Card)
  }

type alias RelativeQuad =
  { self  : RelativeElement
  , right : RelativeElement
  , front : RelativeElement
  , left  : RelativeElement
  }


view : UserId -> Seat -> HandInfo -> ObservableInning -> Html Msg
view userId selfSeat handInfo observableInning =
  case observableInning of
    ObservableDuringInning oinning ->
      let
        submittedQuad = makeSubmittedQuad oinning.startsAt oinning.table
        relQuad = makeRelativeQuad selfSeat oinning.gains submittedQuad
        yourHand = oinning.yourHand
      in
      Svg.svg
        [ SvgA.width (String.fromInt C.svgWidth)
        , SvgA.height (String.fromInt C.svgHeight)
        , SvgA.viewBox ("0 0 " ++ String.fromInt C.svgWidth ++ " " ++ String.fromInt C.svgHeight)
        ]
        (List.concat
          [ displayGains relQuad
          , displayTable relQuad
          , displayHand handInfo yourHand
          ])

    ObservableInningEnd gainsQuad ->
      let
        submittedQuad = { east = [], south = [], west = [], north = [] }
        relQuad = makeRelativeQuad selfSeat gainsQuad submittedQuad
      in
      Svg.svg
        [ SvgA.width (String.fromInt C.svgWidth)
        , SvgA.height (String.fromInt C.svgHeight)
        , SvgA.viewBox ("0 0 " ++ String.fromInt C.svgWidth ++ " " ++ String.fromInt C.svgHeight)
        ]
        (displayGains relQuad)
--        , button [ disabled handInfo.synchronizing, onClick (SendRequest RequireNextInning) ] [ text "次へ" ]
--        ]


displayTable : RelativeQuad -> List (Svg Msg)
displayTable relQuad =
  Debug.todo "displayTable"


displayGains : RelativeQuad -> List (Svg Msg)
displayGains relQuad =
  Debug.todo "displayGains"


showSubmitted : List (ClosedOr Card) -> String
showSubmitted submitted =
  submitted |> List.map (\cardOrClosed ->
    case cardOrClosed of
      Closed    -> "*"
      Open card -> C.stringifyCard card
  ) |> String.join ", "


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


displayHand : HandInfo -> List Card -> List (Svg Msg)
displayHand handInfo cards =
  let
    svgsCard =
      if handInfo.synchronizing then
        cards |> List.indexedMap (\index card ->
          displayCardInHand index Disabled card
            (C.selfHandX + C.verticalTileImageWidth * index)
            C.selfHandY
        )
      else
        case handInfo.maybeIndices of
          Nothing ->
            cards |> List.indexedMap (\index card ->
              displayCardInHand index Disabled card
                (C.selfHandX + C.verticalTileImageWidth * index)
                C.selfHandY
            )

          Just indices ->
            cards |> List.indexedMap (\index card ->
              if indices |> Set.member index then
                displayCardInHand index Selected card
                  (C.selfHandX + C.verticalTileImageWidth * index)
                  (C.selfHandY - C.selectionShift)
              else
                displayCardInHand index NotSelected card
                  (C.selfHandX + C.verticalTileImageWidth * index)
                  C.selfHandY
            )

    svgsButton =
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
          [ button [ disabled (not submittable), onClick (SendRequest SubmitCards) ] [ text "決定" ] ]
  in
  svgsCard ++ svgsButton


displayDecisionButton : Bool -> Int -> Svg Msg
displayDecisionButton submittable numberOfCards =
  let x = C.selfHandX + C.verticalTileImageWidth * numberOfCards + C.decisionButtonGap in
  let
    stySize =
      [ SvgA.x (String.fromInt x)
      , SvgA.y (String.fromInt C.decisionButtonY)
      , SvgA.width (String.fromInt C.decisionButtonWidth)
      , SvgA.height (String.fromInt C.decisionButtonHeight)
      ]
  in
  if submittable then
    Svg.g []
      [ Svg.rect
          (stySize ++ [ SvgA.id "decision-button-enabled" ])
          []
      , Svg.text_
          [ SvgA.x (String.fromInt (x + C.decisionButtonTextDepth))
          , SvgA.y (String.fromInt (C.decisionButtonY + C.decisionButtonWidth // 2))
          , SvgA.textAnchor "middle"
          ]
          [ Svg.text "決定" ]
      , Svg.rect
          (stySize ++
            [ SvgE.onClick (SendRequest SubmitCards)
            , SvgA.id "decision-button-front"
            ])
          []
      ]
  else
    Svg.rect
      (stySize ++ [ SvgA.id "decision-button-disabled" ])
      []


type CardState
  = Disabled
  | Selected
  | NotSelected


displayCardInHand : Int -> CardState -> Card -> Int -> Int -> Svg Msg
displayCardInHand index cardState card x y =
  let
    sty =
      case cardState of
        Disabled    -> SvgA.opacity "0.8"
        Selected    -> SvgE.onClick (UnselectCard index)
        NotSelected -> SvgE.onClick (SelectCard index)
  in
  Svg.image
    [ SvgA.x (String.fromInt x)
    , SvgA.y (String.fromInt y)
    , sty
    , SvgA.xlinkHref (C.standingCardPath card)
    ]
    []


makeRelativeQuad : Seat -> PerSeat (List Card) -> PerSeat (List (ClosedOr Card)) -> RelativeQuad
makeRelativeQuad selfSeat gainsQuad submittedQuad =
  let elem0 = { gains = gainsQuad.east,  submitted = submittedQuad.east } in
  let elem1 = { gains = gainsQuad.south, submitted = submittedQuad.south } in
  let elem2 = { gains = gainsQuad.west,  submitted = submittedQuad.west } in
  let elem3 = { gains = gainsQuad.north, submitted = submittedQuad.north } in
  let elemX = { gains = [], submitted = [] } in -- used only for dummy values
  case selfSeat of
    0 -> { self = elem0, right = elem1, front = elem2, left = elem3 }
    1 -> { self = elem1, right = elem2, front = elem3, left = elem0 }
    2 -> { self = elem2, right = elem3, front = elem0, left = elem1 }
    3 -> { self = elem3, right = elem0, front = elem1, left = elem2 }
    _ -> { self = elemX, right = elemX, front = elemX, left = elemX } -- should never happen


makeSubmittedQuad : Seat -> Table -> PerSeat (List (ClosedOr Card))
makeSubmittedQuad startSeat table =
  let list0 = Game.tableToCards table in
  let
    t =
      case list0 of
        []                        -> { x0 = [], x1 = [], x2 = [], x3 = [] }
        [x0]                      -> { x0 = x0, x1 = [], x2 = [], x3 = [] }
        [x0, x1]                  -> { x0 = x0, x1 = x1, x2 = [], x3 = [] }
        [x0, x1, x2]              -> { x0 = x0, x1 = x1, x2 = x2, x3 = [] }
        x0 :: x1 :: x2 :: x3 :: _ -> { x0 = x0, x1 = x1, x2 = x2, x3 = x3 }
  in
  case startSeat of
    0 -> { east = t.x0, south = t.x1, west = t.x2, north = t.x3 }
    1 -> { east = t.x3, south = t.x0, west = t.x1, north = t.x2 }
    2 -> { east = t.x2, south = t.x3, west = t.x0, north = t.x1 }
    3 -> { east = t.x1, south = t.x2, west = t.x3, north = t.x0 }
    _ -> { east = [], south = [], west = [], north = [] } -- should never happen
