import Html exposing (Html, Attribute, div, label, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { maximumYield : Int
  , ratio : Int
  }

model : Model
model =
  { maximumYield = 300
  , ratio = 17
  }


-- UPDATE

type Msg
  = ChangeMaximumYield String
  | ChangeRatio String

update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeMaximumYield newMaximumYieldString ->
      let
        newMaximumYieldInt = Result.withDefault model.maximumYield (String.toInt newMaximumYieldString)
      in
        { model | maximumYield = newMaximumYieldInt }
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
        [ label [ for "maximumYield" ] [ text "Maximum Yield: " ]
        , input [ id "maximumYield", type_ "number", placeholder "17", value (toString model.maximumYield), onInput ChangeMaximumYield ] []
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
        , text (toString (coffee model.maximumYield model.ratio))
        , text "g"
        ]
      , div []
        [ text "Bloom Water: "
        , text (toString (bloomWater model.maximumYield model.ratio))
        , text "g"
        ]
      , div []
        [ text "Total Water: "
        , text (toString (totalWater model.maximumYield model.ratio))
        , text "g"
        ]
      , div []
        [ text "Expected Yield: "
        , text (toString (expectedYield model.maximumYield model.ratio))
        , text "ml"
        ]
      ]
    ]


-- FUNCTIONS

coffee : Int -> Int -> Int
coffee maximumYield ratio =
  let
    maximumYieldFloat = toFloat maximumYield
    ratioFloat = toFloat ratio
    coffee = maximumYieldFloat / (ratioFloat - 1.5)
  in
    floor coffee

bloomWater : Int -> Int -> Int
bloomWater maximumYield ratio =
  2 * (coffee maximumYield ratio)

totalWater : Int -> Int -> Int
totalWater maximumYield ratio =
  ratio * (coffee maximumYield ratio)

expectedYield : Int -> Int -> Int
expectedYield maximumYield ratio =
  let
    totalWaterUsed = totalWater maximumYield ratio
    coffeeWeight = coffee maximumYield ratio
    groundsWater = floor (toFloat coffeeWeight * 1.5)
  in
    totalWaterUsed - groundsWater
