module Flag exposing (decode)

import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)

import Common exposing (..)


maybeDecoder : Decoder a -> Decoder (Maybe a)
maybeDecoder jd =
  JD.field "type" JD.string
    |> JD.andThen (\s ->
      case s of
        "nothing" ->
          JD.succeed Nothing

        "just" ->
          JD.field "value" jd |> JD.map Just

        _ ->
          JD.fail "other than 'just' or 'nothing'"
    )


flagUserDecoder : Decoder User
flagUserDecoder =
    JD.map3
      (\userId userName belongsTo -> { id = userId, name = userName, belongsTo = belongsTo })
      (JD.field "id" JD.string)
      (JD.field "name" JD.string)
      (JD.field "belongs_to" (maybeDecoder JD.string))



flagDecoder : Decoder (Maybe User)
flagDecoder =
  JD.field "user" (maybeDecoder flagUserDecoder)


decode : String -> Result JD.Error (Maybe User)
decode s =
  JD.decodeString flagDecoder s
