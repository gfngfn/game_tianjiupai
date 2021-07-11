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
    Starting     -> 0
    Wuzun e      -> getExposedSize e
    Wenzun e     -> getExposedSize e
    SingleWen e  -> getExposedSize e
    SingleWu e   -> getExposedSize e
    DoubleWen e  -> getExposedSize e
    DoubleWu e   -> getExposedSize e
    DoubleBoth e -> getExposedSize e
    TripleWen e  -> getExposedSize e
    TripleWu e   -> getExposedSize e
    Quadruple e  -> getExposedSize e


getExposedSize : Exposed a -> Int
getExposedSize exposed =
  1 + List.length exposed.subsequent


isSubmittable : Table -> List Card -> Bool
isSubmittable table cards =
  case table of
    Starting     -> isStartable cards
    Wuzun e      -> List.length cards == 2
    Wenzun e     -> List.length cards == 2
    SingleWen e  -> List.length cards == 1
    SingleWu e   -> List.length cards == 1
    DoubleWen e  -> List.length cards == 2
    DoubleWu e   -> List.length cards == 2
    DoubleBoth e -> List.length cards == 2
    TripleWen e  -> List.length cards == 3
    TripleWu e   -> List.length cards == 3
    Quadruple e  -> List.length cards == 4


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

    Wuzun e ->
      e |> exposedToList 2 (\u ->
        let WuzunUnit = u in
        [Wu { design = True, number = 3 }, Wu { design = True, number = 6 }]
      )

    Wenzun e ->
      e |> exposedToList 2 (\elem ->
        case elem of
          WenzunMinor -> [Wen 1, Wen 1]
          WenzunMajor -> [Wen 2, Wen 2]
      )

    SingleWen e ->
      e |> exposedToList 1 (\wen -> [Wen wen])

    SingleWu e ->
      e |> exposedToList 1 (\wu -> [Wu wu])

    DoubleWen e ->
      e |> exposedToList 2 (\wen -> [Wen wen, Wen wen])

    DoubleWu e ->
      e |> exposedToList 2 (\wunum ->
        [Wu { design = True, number = wunum }, Wu { design = False, number = wunum }]
      )

    DoubleBoth e ->
      e |> exposedToList 2 (\bigd ->
        let ( wen, wunum ) = bigToWenAndWu bigd.main in
        [Wen wen, Wu { design = bigd.design, number = wunum }]
      )

    TripleWen e ->
      e |> exposedToList 3 (\bigd ->
        let ( wen, wunum ) = bigToWenAndWu bigd.main in
        [Wen wen, Wen wen, Wu { design = bigd.design, number = wunum }]
      )

    TripleWu e ->
      e |> exposedToList 3 (\big ->
        let ( wen, wunum ) = bigToWenAndWu big in
        [Wen wen, Wu { design = True, number = wunum }, Wu { design = False, number = wunum }]
      )

    Quadruple e ->
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
    BigA -> ( 8, 5 )
    BigB -> ( 9, 7 )
    BigC -> ( 10, 8 )
    BigD -> ( 11, 9 )


sortCards : List Card -> List Card
sortCards =
  List.sortBy
    (\card ->
      case card of
        Wen wen -> wen
        Wu wu   -> 100 + wu.number * 2 + (if wu.design then 1 else 0)
    )
