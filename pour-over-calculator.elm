import Html exposing (Html, Attribute, div, label, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { yield : Int
  , ratio : Int
  }

model : Model
model =
  { yield = 300
  , ratio = 17
  }


-- UPDATE

type Msg
  = ChangeYield String
  | ChangeRatio String

update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeYield newYieldString ->
      let
        newYieldInt = Result.withDefault model.yield (String.toInt newYieldString)
      in
        { model | yield = newYieldInt }
    ChangeRatio newRatioString ->
      let
        newRatioInt = Result.withDefault model.ratio (String.toInt newRatioString)
      in
        { model | ratio = newRatioInt }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div []
      [ div []
        [ label [ for "yield" ] [ text "Yield: " ]
        , input [ id "yield", type_ "number", placeholder "17", value (toString model.yield), onInput ChangeYield ] []
        , text "ml"
        ]
      , div []
        [ label [ for "ratio" ] [ text "Ratio: " ]
        , input [ id "ratio", type_ "number", placeholder "17", value (toString model.ratio), onInput ChangeRatio ] []
        , text ":1"
        ]
      ]
    , div []
      [ div []
        [ text "Coffee: "
        , text (toString (coffee model.yield model.ratio))
        , text "g"
        ]
      , div []
        [ text "Bloom Water: "
        , text (toString (bloomWater model.yield model.ratio))
        , text "g"
        ]
      , div []
        [ text "Total Water: "
        , text (toString (totalWater model.yield model.ratio))
        , text "g"
        ]
      ]
    ]


-- FUNCTIONS

coffee : Int -> Int -> Int
coffee yield ratio =
  let
    yieldFloat = toFloat yield
    ratioFloat = toFloat ratio
    coffee = yieldFloat / (ratioFloat - 1.5)
  in
    floor coffee

bloomWater : Int -> Int -> Int
bloomWater yield ratio =
  2 * (coffee yield ratio)

totalWater : Int -> Int -> Int
totalWater yield ratio =
  ratio * (coffee yield ratio)
