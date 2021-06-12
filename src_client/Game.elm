module Game exposing
  ( isMyTurn
  , isSubmittable
  , isWaitingLastSubmission
  , bigToWenAndWu
  )

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
          case getNextSubmitterSeat oinning of
            Nothing                -> False
            Just nextSubmitterSeat -> nextSubmitterSeat == mySeat


getMySeat : UserId -> ObservableGameState -> Maybe Seat
getMySeat userId ostate =
  PerSeat.find (\player -> player.user.userId == userId) ostate.meta.players


getNextSubmitterSeat : ObservableInningState -> Maybe Seat
getNextSubmitterSeat oinning =
  let tableSize = getTableSize oinning.table in
  if tableSize == 4 then
      Nothing
  else
      Just (PerSeat.advanceSeat oinning.startsAt tableSize)


isWaitingLastSubmission : ObservableGameState -> Bool
isWaitingLastSubmission ostate =
  case ostate.observableInning of
    ObservableInningEnd _          -> False
    ObservableDuringInning oinning -> 3 == getTableSize oinning.table


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


isSubmittable : Table -> List Card -> Bool
isSubmittable table cards =
  case table of
    Starting          -> isStartable cards
    TrickWuzun e      -> List.length cards == 2
    TrickWenzun e     -> List.length cards == 2
    TrickSingleWen e  -> List.length cards == 1
    TrickSingleWu e   -> List.length cards == 1
    TrickDoubleWen e  -> List.length cards == 2
    TrickDoubleWu e   -> List.length cards == 2
    TrickDoubleBoth e -> List.length cards == 2
    TrickTripleWen e  -> List.length cards == 3
    TrickTripleWu e   -> List.length cards == 3
    TrickQuadruple e  -> List.length cards == 4


isStartable : List Card -> Bool
isStartable cards =
  case sortCards cards of
    [_]                                  -> True
    [Wu 3, Wu 6]                         -> True
    [Wen wen1, Wen wen2]                 -> wen1 == wen2
    [Wu wu1, Wu wu2]                     -> wu1 == wu2
    [Wen wen, Wu wu]                     -> areTheSameBig wen wu
    [Wen wen1, Wen wen2, Wu wu]          -> wen1 == wen2 && areTheSameBig wen1 wu
    [Wen wen, Wu wu1, Wu wu2]            -> wu1 == wu2 && areTheSameBig wen wu1
    [Wen wen1, Wen wen2, Wu wu1, Wu wu2] -> wen1 == wen2 && wu1 == wu2 && areTheSameBig wen1 wu1
    _                                    -> False


areTheSameBig : CardWen -> CardWu -> Bool
areTheSameBig wen wu =
  case ( wen, wu ) of
    ( 11, 9 ) -> True
    ( 10, 8 ) -> True
    ( 9, 7 )  -> True
    ( 8, 5 )  -> True
    _         -> False


bigToWenAndWu : CardBig -> ( CardWen, CardWu )
bigToWenAndWu big =
  case big of
    1 -> ( 8, 5 )
    2 -> ( 9, 7 )
    3 -> ( 10, 8 )
    4 -> ( 11, 9 )
    _ -> ( 0, 0 ) -- Should never happen


sortCards : List Card -> List Card
sortCards =
  List.sortBy
    (\card ->
      case card of
        Wen wen -> wen
        Wu wu   -> 100 + wu
    )
