module ViewTable exposing (view, HandInfo)

import Set exposing (Set)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE

import Models exposing (..)
import Common exposing (..)
import PerSeat exposing (RelativeSeat(..))
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


view : UserId -> Seat -> Seat -> HandInfo -> ObservableInning -> Html Msg
view userId selfSeat parentSeat handInfo observableInning =
  let
    widthText = "min(100%, " ++ (String.fromInt C.svgWidth) ++ "px)"
    viewBoxText = "0 0 " ++ String.fromInt C.svgWidth ++ " " ++ String.fromInt C.svgHeight

    mainElem =
      case observableInning of
        ObservableDuringInning oinning ->
          let
            submittedQuad = makeSubmittedQuad oinning.startsAt oinning.table
            relQuad = makeRelativeQuad selfSeat oinning.gains submittedQuad
            yourHand = oinning.yourHand
            numCardsAtTrickBeginning = List.length yourHand + List.length relQuad.self.submitted
          in
          Svg.svg
            [ SvgA.width widthText
            , SvgA.viewBox viewBoxText
            ]
            (List.concat
              [ displayLeftHand (numCardsAtTrickBeginning - List.length relQuad.left.submitted)
              , displayParentTile (PerSeat.relative { from = selfSeat, target = parentSeat })
              , displayGains relQuad
              , displayTable relQuad
              , displayRightHand (numCardsAtTrickBeginning - List.length relQuad.right.submitted)
              , displayFrontHand (numCardsAtTrickBeginning - List.length relQuad.front.submitted)
              , displayHand handInfo yourHand
              ])

        ObservableInningEnd gainsQuad ->
          let
            submittedQuad = { east = [], south = [], west = [], north = [] }
            relQuad = makeRelativeQuad selfSeat gainsQuad submittedQuad
          in
          Svg.svg
            [ SvgA.width widthText
            , SvgA.viewBox viewBoxText
            ]
            (displayGains relQuad ++
              [ displayButton
                  (not handInfo.synchronizing)
                  (SendRequest RequireNextInning)
                  "次へ"
                  C.goToNextButtonX
                  C.goToNextButtonY
              ])
  in
  div [ class "table-container" ] [ mainElem ]


displayTable : RelativeQuad -> List (Svg Msg)
displayTable relQuad =
  List.concat
    [ displayVerticalSubmitted   C.selfSubmittedX  C.selfSubmittedY  relQuad.self.submitted
    , displayHorizontalSubmitted C.rightSubmittedX C.rightSubmittedY relQuad.right.submitted
    , displayVerticalSubmitted   C.frontSubmittedX C.frontSubmittedY relQuad.front.submitted
    , displayHorizontalSubmitted C.leftSubmittedX  C.leftSubmittedY  relQuad.left.submitted
    ]


displayVerticalSubmitted : Int -> Int -> List (ClosedOr Card) -> List (Svg Msg)
displayVerticalSubmitted x y submitted =
  let num = List.length submitted in
  let x0 = x - (C.verticalTileImageWidth * num // 2) in
  let
    (_, svgacc0 ) =
      submitted |> List.foldl (\cardOrClosed ( index, svgacc ) ->
        let
          svg =
            case cardOrClosed of
              Open card ->
                displayVerticalOpenCard card (x0 + C.verticalTileImageWidth * index) y

              Closed ->
                displayVerticalClosedCard (x0 + C.verticalTileImageWidth * index) y
        in
        ( index + 1, svg :: svgacc )
      ) ( 0, [] )
  in
  List.reverse svgacc0


displayHorizontalSubmitted : Int -> Int -> List (ClosedOr Card) -> List (Svg Msg)
displayHorizontalSubmitted x y submitted =
  let num = List.length submitted in
  let y0 = y - (C.horizontalTileTopHeight * num // 2) in
  let
    ( _, svgacc0 ) =
      submitted |> List.foldl (\cardOrClosed ( index, svgacc ) ->
        let
          svg =
            case cardOrClosed of
              Open card ->
                displayHorizontalOpenCard card x (y0 + C.horizontalTileTopHeight * index)

              Closed ->
                displayHorizontalClosedCard x (y0 + C.horizontalTileTopHeight * index)
        in
        ( index + 1, svg :: svgacc )
      ) ( 0, [] )
  in
  List.reverse svgacc0


displayGains : RelativeQuad -> List (Svg Msg)
displayGains relQuad =
  let selfGains = relQuad.self.gains in
  let selfX = C.selfPileX - C.verticalTileImageWidth * (List.length selfGains) in
  let leftGains = relQuad.left.gains in
  let leftY = C.leftPileY - C.horizontalTileTopHeight * (List.length leftGains) in
  List.concat
    [ displayVerticalPile   selfX        C.selfPileY  selfGains
    , displayHorizontalPile C.rightPileX C.rightPileY relQuad.right.gains
    , displayVerticalPile   C.frontPileX C.frontPileY relQuad.front.gains
    , displayHorizontalPile C.leftPileX  leftY        leftGains
    ]


displayVerticalPile : Int -> Int -> List Card -> List (Svg Msg)
displayVerticalPile x0 y0 gains =
  gains |> List.indexedMap (\index card ->
    let x = x0 + C.verticalTileImageWidth * index in
    [ displayVerticalClosedCard x (y0 + C.tileThickness * 3)
    , displayVerticalClosedCard x (y0 + C.tileThickness * 2)
    , displayVerticalClosedCard x (y0 + C.tileThickness)
    , displayVerticalOpenCard card x y0
    ]
  ) |> List.concat


displayHorizontalPile : Int -> Int -> List Card -> List (Svg Msg)
displayHorizontalPile x0 y0 gains =
  gains |> List.indexedMap (\index card ->
    let y = y0 + C.horizontalTileTopHeight * index in
    [ displayHorizontalClosedCard x0 (y + C.tileThickness * 3)
    , displayHorizontalClosedCard x0 (y + C.tileThickness * 2)
    , displayHorizontalClosedCard x0 (y + C.tileThickness)
    , displayHorizontalOpenCard card x0 y
    ]
  ) |> List.concat


displayFrontHand : Int -> List (Svg Msg)
displayFrontHand numCards =
  let indices = List.range 0 (numCards - 1) in
  let x0 = C.frontHandX - C.verticalTileImageWidth * numCards in
  indices |> List.map (\index ->
    let x = x0 + C.verticalTileImageWidth * index in
    displayClosedStandingCard x C.frontHandY
  )


displayLeftHand : Int -> List (Svg Msg)
displayLeftHand numCards =
  let indices = List.range 0 (numCards - 1) in
  indices |> List.map (\index ->
    let y = C.leftHandY + C.horizontalTileTopHeight * index in
    displayHorizontalStandingCard C.leftHandX y
  )


displayRightHand : Int -> List (Svg Msg)
displayRightHand numCards =
  let indices = List.range 0 (numCards - 1) in
  let y0 = C.rightHandY - C.horizontalTileTopHeight * numCards in
  indices |> List.map (\index ->
    let y = y0 + C.horizontalTileTopHeight * index in
    displayHorizontalStandingCard C.rightHandX y
  )


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
          let numberOfHandCards = List.length cards in
          [ displayButton
              submittable
              (SendRequest SubmitCards)
              "決定"
              (C.selfHandX + C.verticalTileImageWidth * numberOfHandCards + C.decisionButtonGap)
              C.decisionButtonY
          ]
  in
  svgsCard ++ svgsButton


displayButton : Bool -> Msg -> String -> Int -> Int -> Svg Msg
displayButton enabled msg text x y =
  let
    stySize =
      [ SvgA.x (String.fromInt x)
      , SvgA.y (String.fromInt y)
      , SvgA.width (String.fromInt C.svgButtonWidth)
      , SvgA.height (String.fromInt C.svgButtonHeight)
      ]
  in
  if enabled then
    Svg.g []
      [ Svg.rect
          (stySize ++ [ SvgA.class "svg-button-enabled" ])
          []
      , Svg.text_
          [ SvgA.x (String.fromInt (x + C.svgButtonWidth // 2))
          , SvgA.y (String.fromInt (y + C.svgButtonTextDepth))
          , SvgA.textAnchor "middle"
          , SvgA.class "svg-button-text-enabled"
          ]
          [ Svg.text text ]
      , Svg.rect
          (stySize ++
            [ SvgE.onClick msg
            , SvgA.class "svg-button-front"
            ])
          []
      ]
  else
    Svg.g []
      [ Svg.rect
          (stySize ++ [ SvgA.class "svg-button-disabled" ])
          []
      , Svg.text_
          [ SvgA.x (String.fromInt (x + C.svgButtonWidth // 2))
          , SvgA.y (String.fromInt (y + C.svgButtonTextDepth))
          , SvgA.textAnchor "middle"
          , SvgA.class "svg-button-text-disabled"
          ]
          [ Svg.text text ]
      ]


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


displayClosedStandingCard : Int -> Int -> Svg Msg
displayClosedStandingCard x y =
  svgImage ( x, y ) C.verticalClosedStandingCardPath


displayHorizontalStandingCard : Int -> Int -> Svg Msg
displayHorizontalStandingCard x y =
  svgImage ( x, y ) C.horizontalClosedStandingCardPath


displayHorizontalOpenCard : Card -> Int -> Int -> Svg Msg
displayHorizontalOpenCard card x y =
  svgImage ( x, y ) (C.horizontalOpenCardPath card)


displayHorizontalClosedCard : Int -> Int -> Svg Msg
displayHorizontalClosedCard x y =
  svgImage ( x, y ) C.horizontalClosedCardPath


displayVerticalOpenCard : Card -> Int -> Int -> Svg Msg
displayVerticalOpenCard card x y =
  svgImage ( x, y ) (C.verticalOpenCardPath card)


displayVerticalClosedCard : Int -> Int -> Svg Msg
displayVerticalClosedCard x y =
  svgImage ( x, y ) C.verticalClosedCardPath


displayParentTile : RelativeSeat -> List (Svg Msg)
displayParentTile relParentSeat =
  case relParentSeat of
    Self  -> [ svgImage ( C.selfParentTileX,  C.selfParentTileY  ) C.selfParentTilePath  ]
    Right -> [ svgImage ( C.rightParentTileX, C.rightParentTileY ) C.rightParentTilePath ]
    Front -> [ svgImage ( C.frontParentTileX, C.frontParentTileY ) C.frontParentTilePath ]
    Left  -> [ svgImage ( C.leftParentTileX,  C.leftParentTileY  ) C.leftParentTilePath  ]


svgImage : ( Int, Int ) -> String -> Svg Msg
svgImage ( x, y ) path =
  Svg.image
    [ SvgA.x (String.fromInt x)
    , SvgA.y (String.fromInt y)
    , SvgA.xlinkHref path
    ]
    []


makeRelativeQuad : Seat -> PerSeat (List Card) -> PerSeat (List (ClosedOr Card)) -> RelativeQuad
makeRelativeQuad selfSeat gainsQuad submittedQuad =
  let elem0 = { gains = gainsQuad.east,  submitted = submittedQuad.east } in
  let elem1 = { gains = gainsQuad.south, submitted = submittedQuad.south } in
  let elem2 = { gains = gainsQuad.west,  submitted = submittedQuad.west } in
  let elem3 = { gains = gainsQuad.north, submitted = submittedQuad.north } in
  case selfSeat of
    SeatA -> { self = elem0, right = elem1, front = elem2, left = elem3 }
    SeatB -> { self = elem1, right = elem2, front = elem3, left = elem0 }
    SeatC -> { self = elem2, right = elem3, front = elem0, left = elem1 }
    SeatD -> { self = elem3, right = elem0, front = elem1, left = elem2 }


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
    SeatA -> { east = t.x0, south = t.x1, west = t.x2, north = t.x3 }
    SeatB -> { east = t.x3, south = t.x0, west = t.x1, north = t.x2 }
    SeatC -> { east = t.x2, south = t.x3, west = t.x0, north = t.x1 }
    SeatD -> { east = t.x1, south = t.x2, west = t.x3, north = t.x0 }
