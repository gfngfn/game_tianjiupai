module Flag exposing (decode)

import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)

import Common exposing (..)


flagUserDecoder : Decoder User
flagUserDecoder =
    JD.map2
      (\userId userName -> { id = userId, name = userName })
      (JD.field "id" JD.string)
      (JD.field "name" JD.string)


flagMaybeUserDecoder : Decoder (Maybe User)
flagMaybeUserDecoder =
  JD.field "type" JD.string
    |> JD.andThen (\s ->
      case s of
        "nothing" ->
          JD.succeed Nothing

        "just" ->
          JD.field "value" flagUserDecoder |> JD.map Just

        _ ->
          JD.fail "other than 'just' or 'nothing'"
    )


flagDecoder : Decoder (Maybe User)
flagDecoder =
  JD.field "user" flagMaybeUserDecoder


decode : String -> Result JD.Error (Maybe User)
decode s =
  JD.decodeString flagDecoder s
