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
        relQuad = makeRelativeQuad selfSeat oinning.gains oinning.startsAt oinning.table
        yourHand = oinning.yourHand
      in
      div []
        [ div [] [ text "ObservableDuringInning" ]
        , showGainsOfRelativeQuad relQuad
        , showTable relQuad
        , showHand handInfo yourHand
        ]

    ObservableInningEnd gainsQuad ->
      div []
        [ div [] [ text "ObservableInningEnd" ]
--      , showGainsQuad gainsQuad -- TODO
        , button [ disabled handInfo.synchronizing, onClick (SendRequest RequireNextInning) ] [ text "次へ" ]
        ]


makeRelativeQuad : Seat -> PerSeat (List Card) -> Seat -> Table -> RelativeQuad
makeRelativeQuad selfSeat gainsQuad startSeat table =
  let submittedQuad = makeSubmittedQuad startSeat table in
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
  let
    list0 : List (List (ClosedOr Card))
    list0 =
      case table of
        Starting ->
          []

        TrickWuzun e ->
          e |> exposedToList 2 (\u -> let Unit = u in [Wu 3, Wu 6])

        TrickWenzun e ->
          e |> exposedToList 2 (\b ->
            case b of
              False -> [Wen 1, Wen 1]
              True  -> [Wen 2, Wen 2]
          )

        TrickSingleWen e ->
          e |> exposedToList 1 (\wen -> [Wen wen])

        TrickSingleWu e ->
          e |> exposedToList 1 (\wu -> [Wu wu])

        TrickDoubleWen e ->
          e |> exposedToList 2 (\wen -> [Wen wen, Wen wen])

        TrickDoubleWu e ->
          e |> exposedToList 2 (\wu -> [Wu wu, Wu wu])

        TrickDoubleBoth e ->
          e |> exposedToList 2 (\big ->
            let ( wen, wu ) = Game.bigToWenAndWu big in
            [Wen wen, Wu wu]
          )

        TrickTripleWen e ->
          e |> exposedToList 3 (\big ->
            let ( wen, wu ) = Game.bigToWenAndWu big in
            [Wen wen, Wen wen, Wu wu]
          )

        TrickTripleWu e ->
          e |> exposedToList 3 (\big ->
            let ( wen, wu ) = Game.bigToWenAndWu big in
            [Wen wen, Wu wu, Wu wu]
          )

        TrickQuadruple e ->
          e |> exposedToList 4 (\big ->
            let ( wen, wu ) = Game.bigToWenAndWu big in
            [Wen wen, Wu wu, Wu wu, Wu wu]
          )
  in
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


exposedToList : Int -> (a -> List Card) -> Exposed a -> List (List (ClosedOr Card))
exposedToList n f e =
  (Open e.first :: e.subsequent) |> List.map (\xOrClosed ->
    case xOrClosed of
      Open x -> f x |> List.map (\y -> Open y)
      Closed -> List.repeat n Closed
  )


showTable : RelativeQuad -> Html Msg
showTable relQuad =
  div []
    [ div [] [ text "場:" ]
    , ul []
        [ li [] [ text ("自 " ++ showSubmitted relQuad.self.submitted) ]
        , li [] [ text ("右 " ++ showSubmitted relQuad.right.submitted) ]
        , li [] [ text ("奥 " ++ showSubmitted relQuad.front.submitted) ]
        , li [] [ text ("左 " ++ showSubmitted relQuad.left.submitted) ]
        ]
    ]

showSubmitted : List (ClosedOr Card) -> String
showSubmitted submitted =
  submitted |> List.map (\cardOrClosed ->
    case cardOrClosed of
      Closed    -> "*"
      Open card -> showCard card
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


showGainsOfRelativeQuad : RelativeQuad -> Html Msg
showGainsOfRelativeQuad relQuad =
  div []
    [ div []
        [ text "山:" ]
    , ul []
        [ li [] [ text ("自 " ++ showCards relQuad.self.gains) ]
        , li [] [ text ("右 " ++ showCards relQuad.right.gains) ]
        , li [] [ text ("奥 " ++ showCards relQuad.front.gains) ]
        , li [] [ text ("左 " ++ showCards relQuad.left.gains) ]
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
    ([ div [] [ text "手牌:" ], ul [] elems ] ++ buttonElems)


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
