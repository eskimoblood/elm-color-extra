module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, type_, min, max)
import Html.Events exposing (onClick, onInput)
import Color exposing (..)
import Color.Gradient exposing (..)
import Color.Convert exposing (colorToCssRgb)
import Plot.Types exposing (..)
import Plot exposing (..)
import Plot.Line as Line
import Plot.Axis as Axis


main =
    Html.beginnerProgram { model = optOut, update = update, view = view }


type alias Setting =
    { r : Float, g : Float, b : Float }



-- MODEL


type alias Model =
    { offset : Setting
    , amp : Setting
    , fmod : Setting
    , phase : Setting
    }


optOut : Model
optOut =
    Model { r = 0.5, g = 0.5, b = 0.5 } { r = 0.5, g = 0.5, b = 0.5 } { r = 1, g = 1, b = 1 } { r = 0, g = 0.333, b = 0.666 }


type Msg
    = UpdateOffset Setting
    | UpdateFmod Setting
    | UpdateAmp Setting
    | UpdatePhase Setting


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateOffset setting ->
            { model | offset = setting }

        UpdateAmp setting ->
            { model | amp = setting }

        UpdateFmod setting ->
            { model | fmod = setting }

        UpdatePhase setting ->
            { model | phase = setting }


toGradientSetting : Setting -> CosineGradientSetting
toGradientSetting { r, g, b } =
    ( r, g, b )


view : Model -> Html Msg
view model =
    let
        offset =
            model.offset

        amp =
            model.amp

        fmod =
            model.fmod

        phase =
            model.phase
    in
        div []
            [ div [ Html.Attributes.style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
                [ group "Offset" offset UpdateOffset
                , group "Amp" amp UpdateAmp
                , group "Fmod" fmod UpdateFmod
                , group "Phase" phase UpdatePhase
                ]
            , ul
                [ Html.Attributes.style [ ( "listStyle", "none" ), ( "display", "flex" ), ( "flex-direction", "row" ) ]
                ]
                (List.map item (cosineGradient (toGradientSetting offset) (toGradientSetting amp) (toGradientSetting fmod) (toGradientSetting phase) 300))
            , plot
                [ size ( 600, 300 )
                , margin ( 100, 100, 40, 100 )
                , id "PlotHint"
                , Plot.style [ ( "position", "relative" ) ]
                , domainLowest (always 0)
                , domainHighest (always 1)
                ]
                [ getLine offset.r amp.r fmod.r phase.r "#ff0000"
                , getLine offset.g amp.g fmod.g phase.g "#00ff00"
                , getLine offset.b amp.b fmod.b phase.b "#0000ff"
                ]
            ]


getLine offset amp fmod phase lineColor =
    line
        [ Line.stroke lineColor
        , Line.strokeWidth 1
        ]
        (List.map
            (toFloat >> (\i -> ( i / 100, (calcCosine offset amp fmod phase (i / 100)) )))
            (List.range 0 100)
        )


item : Color -> Html msg
item cl =
    li
        [ Html.Attributes.style
            [ ( "backgroundColor", colorToCssRgb cl )
            , ( "width", "2px" )
            , ( "height", "40px" )
            ]
        ]
        []


group title data msg =
    div
        [ Html.Attributes.style [ ( "flex", "1" ) ] ]
        [ h4 [] [ (text title) ]
        , slider data.r (String.toFloat >> Result.withDefault 0 >> \a -> msg ({ data | r = a }))
        , slider data.g (String.toFloat >> Result.withDefault 0 >> \a -> msg ({ data | g = a }))
        , slider data.b (String.toFloat >> Result.withDefault 0 >> \a -> msg ({ data | b = a }))
        ]


slider : Float -> (String -> Msg) -> Html Msg
slider value msg =
    label
        [ Html.Attributes.style [ ( "margin", "10px 0" ), ( "width", "100%" ) ]
        ]
        [ input
            [ type_ "range"
            , Html.Attributes.min "-3.14"
            , Html.Attributes.max "3.14"
            , Html.Attributes.step "0.01"
            , Html.Attributes.value (toString value)
            , onInput msg
            ]
            []
        , input
            [ type_ "number", Html.Attributes.value (toString value), onInput msg, Html.Attributes.step "0.1" ]
            []
        ]


calcCosine : Float -> Float -> Float -> Float -> Float -> Float
calcCosine a b c d t =
    clamp 0 1 (a + b * cos (pi * 2 * (c * t + d)))
