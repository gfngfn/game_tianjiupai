module Game exposing (isMyTurn)

import Models exposing (..)
import PerSeat


isMyTurn : UserId -> ObservableGameState -> Bool
isMyTurn userId ostate =
  case getMySeat userId ostate of
    Nothing ->
      False

    Just mySeat ->
      case ostate.observableInning of
        ObservableInningEnd _ ->
          False

        ObservableDuringInning oinning ->
          let nextSubmitterSeat = getNextSubmitterSeat oinning in
          nextSubmitterSeat == mySeat


getMySeat : UserId -> ObservableGameState -> Maybe Seat
getMySeat userId ostate =
  PerSeat.find (\player -> player.userId == userId) ostate.meta.players


getNextSubmitterSeat : ObservableInningState -> Seat
getNextSubmitterSeat oinning =
  let tableSize = getTableSize oinning.table in
  PerSeat.advanceSeat oinning.startsAt tableSize


getTableSize : Table -> Int
getTableSize table =
  case table of
    Starting          -> 0
    TrickWuzun e      -> getExposedSize e
    TrickWenzun e     -> getExposedSize e
    TrickSingleWen e  -> getExposedSize e
    TrickSingleWu e   -> getExposedSize e
    TrickDoubleWen e  -> getExposedSize e
    TrickDoubleWu e   -> getExposedSize e
    TrickDoubleBoth e -> getExposedSize e
    TrickTripleWen e  -> getExposedSize e
    TrickTripleWu e   -> getExposedSize e
    TrickQuadruple e  -> getExposedSize e


getExposedSize : Exposed a -> Int
getExposedSize exposed =
  1 + List.length exposed.subsequent
