module Flag exposing (decode)

import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)

import Common exposing (..)
import Format exposing (..)


decode : String -> Result JD.Error (Maybe User)
decode s =
  JD.decodeString flagDecoder s
