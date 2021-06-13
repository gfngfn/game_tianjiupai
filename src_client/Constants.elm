module Constants exposing (..)

import Models exposing (..)


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

selectionShift : Int
selectionShift = 10

decisionButtonY : Int
decisionButtonY = 620 - decisionButtonHeight

decisionButtonGap : Int
decisionButtonGap = 20

decisionButtonWidth : Int
decisionButtonWidth = 70

decisionButtonHeight : Int
decisionButtonHeight = 40

decisionButtonTextDepth : Int
decisionButtonTextDepth = 32

selfHandX : Int
selfHandX = 190

selfHandY : Int
selfHandY = 515

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
leftPileY = 570

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


standingCardPath : Card -> String
standingCardPath card =
  ("assets/stand-" ++ stringifyCard card ++ ".png")


stringifyCard : Card -> String
stringifyCard card =
  case card of
    Wen wen -> stringifyWen wen
    Wu wu   -> stringifyWu wu


stringifyBig : CardBig -> String
stringifyBig big =
  "big" ++ String.fromInt big


stringifyWen : CardWen -> String
stringifyWen wen =
  "wen" ++ String.fromInt wen


stringifyWu : CardWu -> String
stringifyWu wu =
  "wu" ++ String.fromInt wu.number ++ (if wu.design then "t" else "f")
