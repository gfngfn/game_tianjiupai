module Main exposing (..)

import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)
import Url exposing (Url)
import Http
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type alias Flags = String

type alias UserId = String

type alias UserName = String

type alias InputModel =
  { userName : String
  }

type alias Model =
  { navigationKey : Navigation.Key
  , message       : String
  , inputs        : InputModel
  , user          : Maybe User
  }

type Request
  = CreateUser

type Response
  = UserCreated UserName (Result Http.Error UserId)

type Msg
  = UrlChange Url
  | UrlRequest UrlRequest
  | UpdateUserName UserName
  | Send Request
  | Receive Response

type alias User =
  { id   : UserId
  , name : UserName
  }


host : String
host =
  "localhost:8080"


main =
  Browser.application
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , onUrlChange   = UrlChange
    , onUrlRequest  = UrlRequest
    , view          = view
    }


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


initInputs : InputModel
initInputs =
  { userName = "" }


init : String -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flagString url navKey =
  let
    maybeUser : Maybe User
    maybeUser =
      case JD.decodeString flagDecoder flagString of
          Ok(maybeUser0) -> maybeUser0
          Err(_)         -> Nothing

    model : Model
    model =
      { navigationKey = navKey
      , message       = "flags: " ++ flagString
      , inputs        = initInputs
      , user          = maybeUser
      }
  in
  ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateUserName userName ->
      let inputs = model.inputs in
      ( { model | inputs = { inputs | userName = userName } }, Cmd.none )

    Send req ->
      let cmd = makeCmdFromRequest req model in
      ( model, Cmd.map Receive cmd )

    Receive response ->
      case response of
        UserCreated userName result ->
          case result of
            Ok userId ->
              let user = { id = userId, name = userName } in
              ( { model | user = Just user }, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

    UrlChange url ->
      ( model, Cmd.none )

    UrlRequest urlRequest ->
      case urlRequest of
        Browser.External href ->
          ( model, Navigation.load href )

        Browser.Internal url ->
          ( model, Navigation.pushUrl model.navigationKey (Url.toString url) )


view : Model -> Browser.Document Msg
view model =
  { title = "tianjiupai"
  , body  = viewBody model
  }


viewBody : Model -> List (Html Msg)
viewBody model =
  let
    elemMain =
      case model.user of
        Nothing ->
          div []
            [ input
                [ type_ "text"
                , placeholder "username"
                , value model.inputs.userName
                , onInput UpdateUserName
                ] []
            , button
                [ onClick (Send CreateUser) ]
                [ text "start" ]
            ]

        Just user ->
          div []
            [ text ("Hi, " ++ user.name ++ "! (your user ID: " ++ user.id ++ ")") ]
  in
  [ div []
      [ div [] [ text model.message ]
      , elemMain
      ]
  ]


makeErrorMessage : Http.Error -> String
makeErrorMessage err =
  let
    msg =
      case err of
        Http.BadUrl s     -> "bad URL; " ++ s
        Http.Timeout      -> "request timeout"
        Http.BadBody s    -> "bad body; " ++ s
        Http.NetworkError -> "network error"
        Http.BadStatus n  -> "bad status; " ++ String.fromInt n
  in
  "[error] " ++ msg


makeCmdFromRequest : Request -> Model -> Cmd Response
makeCmdFromRequest req model =
  case req of
    CreateUser ->
      let userName = model.inputs.userName in
      Http.post
        { url    = "http://" ++ host ++ "/users"
        , body   = Http.jsonBody (JE.object [ ( "user_name", JE.string userName ) ])
        , expect = Http.expectJson (UserCreated userName) (JD.field "user_id" JD.string)
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
