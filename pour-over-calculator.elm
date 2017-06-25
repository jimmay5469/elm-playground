import Html exposing (Html, Attribute, div, label, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { maximumYield : Int
  , ratio : Int
  , lossRatio : Float
  }

model : Model
model =
  { maximumYield = 300
  , ratio = 17
  , lossRatio = 1.5
  }


-- UPDATE

type Msg
  = ChangeMaximumYield String
  | ChangeRatio String
  | ChangeLossRatio String

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
    ChangeLossRatio newLossRatioString ->
      let
        newLossRatioFloat = Result.withDefault model.lossRatio (String.toFloat newLossRatioString)
      in
        { model | lossRatio = newLossRatioFloat }


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
      , div []
        [ label [ for "lossRatio" ] [ text "Loss Ratio: " ]
        , input [ id "lossRatio", type_ "number", placeholder "1.5", value (toString model.lossRatio), onInput ChangeLossRatio ] []
        , text ":1"
        ]
      ]
    , div []
      [ div []
        [ text "Coffee: "
        , text (toString (coffee model.maximumYield model.ratio model.lossRatio))
        , text "g"
        ]
      , div []
        [ text "Bloom Water: "
        , text (toString (bloomWater model.maximumYield model.ratio model.lossRatio))
        , text "g"
        ]
      , div []
        [ text "Total Water: "
        , text (toString (totalWater model.maximumYield model.ratio model.lossRatio))
        , text "g"
        ]
      , div []
        [ text "Expected Yield: "
        , text (toString (expectedYield model.maximumYield model.ratio model.lossRatio))
        , text "ml"
        ]
      ]
    ]


-- FUNCTIONS

coffee : Int -> Int -> Float -> Int
coffee maximumYield ratio lossRatio =
  let
    maximumYieldFloat = toFloat maximumYield
    ratioFloat = toFloat ratio
    coffee = maximumYieldFloat / (ratioFloat - lossRatio)
  in
    floor coffee

bloomWater : Int -> Int -> Float -> Int
bloomWater maximumYield ratio lossRatio =
  2 * (coffee maximumYield ratio lossRatio)

totalWater : Int -> Int -> Float -> Int
totalWater maximumYield ratio lossRatio =
  ratio * (coffee maximumYield ratio lossRatio)

expectedYield : Int -> Int -> Float -> Int
expectedYield maximumYield ratio lossRatio =
  let
    totalWaterUsed = totalWater maximumYield ratio lossRatio
    coffeeWeight = coffee maximumYield ratio lossRatio
    groundsWater = floor (lossRatio * (toFloat coffeeWeight))
  in
    totalWaterUsed - groundsWater
