import Html exposing (Html, button, div, text)
import Http
import Json.Decode as Decode

token = ""

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init =
  ("Loading..."
  , getFacebookPageName "216386225070434"
  )

type Msg = UpdatePageName (Result Http.Error String)

update msg model =
  case msg of
    UpdatePageName (Ok name) ->
      (name, Cmd.none)
    UpdatePageName (Err _) ->
      ("Ahhhhhhhh!", Cmd.none)

view  model =
  div [] [ text (model) ]

subscriptions model =
  Sub.none

getFacebookPageName id =
  let
    url = "https://graph.facebook.com/v2.9/" ++ id
    request = Http.request
      { method = "GET"
      , headers = [Http.header "Authorization" ("Bearer " ++ token)]
      , url = url
      , expect = Http.expectJson decodeFacebookPageName
      , body = Http.emptyBody
      , timeout = Nothing
      , withCredentials = False
      }
  in
    Http.send UpdatePageName request

decodeFacebookPageName =
  Decode.at ["name"] Decode.string
