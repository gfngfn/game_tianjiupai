module Constants exposing (..)

import Models exposing (..)


maximumNumInnings : Int
maximumNumInnings = 1


trickLastTimeMs : Float
trickLastTimeMs = 1500.0


heartbeatIntervalMs : Float
heartbeatIntervalMs = 10000.0


svgWidth : Int
svgWidth = 700

svgHeight : Int
svgHeight = 670

verticalTileImageWidth : Int
verticalTileImageWidth = 40

verticalTileImageHeight : Int
verticalTileImageHeight = 105

horizontalTileTopHeight : Int
horizontalTileTopHeight = 34

tileThickness : Int
tileThickness = 16


horizontalStandingTileThickness : Int
horizontalStandingTileThickness = 28

selectionShift : Int
selectionShift = 10

decisionButtonY : Int
decisionButtonY = 620 - svgButtonHeight

decisionButtonGap : Int
decisionButtonGap = 15

goToNextButtonX : Int
goToNextButtonX = 350 - svgButtonWidth // 2

goToNextButtonY : Int
goToNextButtonY = 340 - svgButtonHeight // 2

roomCloseButtonX : Int
roomCloseButtonX = 350

roomCloseButtonY : Int
roomCloseButtonY = 470 - svgButtonHeight // 2

roomCloseTextX : Int
roomCloseTextX = roomCloseButtonX

roomCloseTextY : Int
roomCloseTextY = 230

roomCloseTextLeading : Int
roomCloseTextLeading = 32

svgButtonWidth : Int
svgButtonWidth = 70

svgButtonHeight : Int
svgButtonHeight = 40

svgButtonTextDepth : Int
svgButtonTextDepth = 25

selfHandX : Int
selfHandX = 190

selfHandY : Int
selfHandY = 515

rightHandX : Int
rightHandX = svgWidth - leftHandX - horizontalStandingTileThickness

rightHandY : Int
rightHandY = leftHandY + horizontalTileTopHeight * 8

frontHandX : Int
frontHandX = selfHandX + verticalTileImageWidth * 8

frontHandY : Int
frontHandY = 50

leftHandX : Int
leftHandX = 50

leftHandY : Int
leftHandY = 170

selfPileX : Int
selfPileX = 610

selfPileY : Int
selfPileY = 495

rightPileX : Int
rightPileX = 575

rightPileY : Int
rightPileY = 100

frontPileX : Int
frontPileX = 90

frontPileY : Int
frontPileY = 20

leftPileX : Int
leftPileX = 21

leftPileY : Int
leftPileY = 530

selfSubmittedX : Int
selfSubmittedX = 350

selfSubmittedY : Int
selfSubmittedY = 370

rightSubmittedX : Int
rightSubmittedX = 450

rightSubmittedY : Int
rightSubmittedY = 340

frontSubmittedX : Int
frontSubmittedX = 350

frontSubmittedY : Int
frontSubmittedY = 195

leftSubmittedX : Int
leftSubmittedX = 146

leftSubmittedY : Int
leftSubmittedY = 340

selfParentTileX : Int
selfParentTileX = 440

selfParentTileY : Int
selfParentTileY = 435

rightParentTileX : Int
rightParentTileX = 480

rightParentTileY : Int
rightParentTileY = 180

frontParentTileX : Int
frontParentTileX = 138

frontParentTileY : Int
frontParentTileY = 190

leftParentTileX : Int
leftParentTileX = 135

leftParentTileY : Int
leftParentTileY = 430

standingCardPath : Card -> String
standingCardPath card =
  "assets/stand-" ++ stringifyCard card ++ ".png"


horizontalOpenCardPath : Card -> String
horizontalOpenCardPath card =
  "assets/openh-" ++ stringifyCard card ++ ".png"


horizontalClosedCardPath : String
horizontalClosedCardPath =
  "assets/closeh.png"


verticalOpenCardPath : Card -> String
verticalOpenCardPath card =
  "assets/openv-" ++ stringifyCard card ++ ".png"


verticalClosedCardPath : String
verticalClosedCardPath =
  "assets/closev.png"


verticalClosedStandingCardPath : String
verticalClosedStandingCardPath =
  "assets/standv.png"


horizontalClosedStandingCardPath : String
horizontalClosedStandingCardPath =
  "assets/standh.png"


selfParentTilePath : String
selfParentTilePath =
  "assets/parent-self.png"


rightParentTilePath : String
rightParentTilePath =
  "assets/parent-right.png"


frontParentTilePath : String
frontParentTilePath =
  "assets/parent-front.png"


leftParentTilePath : String
leftParentTilePath =
  "assets/parent-left.png"


directionImagePath : Seat -> String
directionImagePath seat =
  "assets/direction" ++ stringifySeat seat ++ ".png"


stringifyCard : Card -> String
stringifyCard card =
  case card of
    Wen wen -> stringifyWen wen
    Wu wu   -> stringifyWu wu


stringifyBig : CardBig -> String
stringifyBig big =
  case big of
    BigA -> "big1"
    BigB -> "big2"
    BigC -> "big3"
    BigD -> "big4"


stringifyWen : CardWen -> String
stringifyWen wen =
  "wen" ++ String.fromInt wen


stringifyWu : CardWu -> String
stringifyWu wu =
  "wu" ++ String.fromInt wu.number ++ (if wu.design then "t" else "f")


stringifySeat : Seat -> String
stringifySeat seat =
  case seat of
    SeatA -> "1"
    SeatB -> "2"
    SeatC -> "3"
    SeatD -> "4"
