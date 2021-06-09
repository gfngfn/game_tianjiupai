module PerSeat exposing (find, advanceSeat)

import Models exposing (..)


find : (a -> Bool) -> PerSeat a -> Maybe Seat
find f p =
  if f p.east then
    Just 0
  else if f p.south then
    Just 1
  else if f p.west then
    Just 2
  else if f p.north then
    Just 3
  else
    Nothing


advanceSeat : Seat -> Int -> Seat
advanceSeat seat n =
  (seat + n) |> modBy 4
