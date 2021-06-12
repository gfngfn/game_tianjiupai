module Game exposing
  ( isMyTurn
  , isSubmittable
  , isWaitingLastSubmission
  , tableToCards
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
    [Wen wen1, Wen wen2]                 -> wen1 == wen2
    [Wu wu1, Wu wu2]                     -> wu1.number == wu2.number || (wu1.number == 3 && wu2.number == 6)
    [Wen wen, Wu wu]                     -> areTheSameBig wen wu.number
    [Wen wen1, Wen wen2, Wu wu]          -> wen1 == wen2 && areTheSameBig wen1 wu.number
    [Wen wen, Wu wu1, Wu wu2]            -> wu1.number == wu2.number && areTheSameBig wen wu1.number
    [Wen wen1, Wen wen2, Wu wu1, Wu wu2] -> wen1 == wen2 && wu1.number == wu2.number && areTheSameBig wen1 wu1.number
    _                                    -> False


tableToCards : Table -> List (List (ClosedOr Card))
tableToCards table =
  case table of
    Starting ->
      []

    TrickWuzun e ->
      e |> exposedToList 2 (\u ->
        let Unit = u in
        [Wu { design = True, number = 3 }, Wu { design = True, number = 6 }]
      )

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
      e |> exposedToList 2 (\wunum ->
        [Wu { design = True, number = wunum }, Wu { design = True, number = wunum }]
      )

    TrickDoubleBoth e ->
      e |> exposedToList 2 (\bigd ->
        let ( wen, wunum ) = bigToWenAndWu bigd.main in
        [Wen wen, Wu { design = bigd.design, number = wunum }]
      )

    TrickTripleWen e ->
      e |> exposedToList 3 (\bigd ->
        let ( wen, wunum ) = bigToWenAndWu bigd.main in
        [Wen wen, Wen wen, Wu { design = bigd.design, number = wunum }]
      )

    TrickTripleWu e ->
      e |> exposedToList 3 (\big ->
        let ( wen, wunum ) = bigToWenAndWu big in
        [Wen wen, Wu { design = True, number = wunum }, Wu { design = False, number = wunum }]
      )

    TrickQuadruple e ->
      e |> exposedToList 4 (\big ->
        let ( wen, wunum ) = bigToWenAndWu big in
        [Wen wen, Wen wen, Wu { design = True, number = wunum }, Wu { design = False, number = wunum }]
      )


exposedToList : Int -> (a -> List Card) -> Exposed a -> List (List (ClosedOr Card))
exposedToList n f e =
  (Open e.first :: e.subsequent) |> List.map (\xOrClosed ->
    case xOrClosed of
      Open x -> f x |> List.map (\y -> Open y)
      Closed -> List.repeat n Closed
  )


areTheSameBig : CardWen -> CardWuNumber -> Bool
areTheSameBig wen wu =
  case ( wen, wu ) of
    ( 11, 9 ) -> True
    ( 10, 8 ) -> True
    ( 9, 7 )  -> True
    ( 8, 5 )  -> True
    _         -> False


bigToWenAndWu : CardBig -> ( CardWen, CardWuNumber )
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
        Wu wu   -> 100 + wu.number
    )
