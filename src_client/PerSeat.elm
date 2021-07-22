module PerSeat exposing
  ( find
  , update
  , advanceSeat
  , RelativeSeat(..)
  , relative
  )

import Models exposing (..)


find : (a -> Bool) -> PerSeat a -> Maybe Seat
find f p =
  if f p.east then
    Just SeatA
  else if f p.south then
    Just SeatB
  else if f p.west then
    Just SeatC
  else if f p.north then
    Just SeatD
  else
    Nothing


update : Seat -> a -> PerSeat a -> PerSeat a
update seat x p =
  case seat of
    SeatA -> { p | east  = x }
    SeatB -> { p | south = x }
    SeatC -> { p | west  = x }
    SeatD -> { p | north = x }


succSeat : Seat -> Seat
succSeat seat =
  case seat of
    SeatA -> SeatB
    SeatB -> SeatC
    SeatC -> SeatD
    SeatD -> SeatA


advanceSeat : Seat -> Int -> Seat
advanceSeat seat n =
  if n < 0 then
    advanceSeat seat (n + 4)
  else if n == 0 then
    seat
  else
    advanceSeat (succSeat seat) (n - 1)


type RelativeSeat
  = Self
  | Right
  | Front
  | Left


relative : { from : Seat, target : Seat } -> RelativeSeat
relative r =
  let
    self = r.from
    target = r.target
  in
  if target == self then
    Self
  else
    let right = succSeat self in
    if target == right then
      Right
    else
      let front = succSeat right in
      if target == front then
        Front
      else
        Left
