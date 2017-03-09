module Manipulate exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, type_, min, max)
import Html.Events exposing (onClick, onInput)
import Color exposing (..)
import Color.Manipulate exposing (..)
import Color.Convert exposing (..)
import Color.Accessibility exposing (..)


main =
    Html.beginnerProgram { model = model, update = update, view = view }


type alias Model =
    { darken : ( Float, Color.Color, Color.Color )
    , lighten : ( Float, Color.Color, Color.Color )
    , saturate : ( Float, Color.Color, Color.Color )
    , desaturate : ( Float, Color.Color, Color.Color )
    , rotateHue : ( Float, Color.Color, Color.Color )
    , fadeIn : ( Float, Color.Color, Color.Color )
    , fadeOut : ( Float, Color.Color, Color.Color )
    , grayscale : ( Color.Color, Color.Color )
    , scaleHsl : ( ( Float, Float, Float ), Color.Color, Color.Color )
    , mix : ( Color.Color, Color.Color, Color.Color )
    , weightedMix : ( Color.Color, Color.Color, Float, Color.Color )
    }


update1 v1 fn =
    ( v1, fn v1 )


update2 v1 v2 fn =
    ( v1, v2, fn v1 v2 )


update3 v1 v2 v3 fn =
    ( v1, v2, v3, fn v1 v2 v3 )


model : Model
model =
    { darken = update2 0.1 (rgb 255 0 0) darken
    , lighten = update2 0.1 (rgb 255 0 0) lighten
    , saturate = update2 0.5 (rgb 180 69 66) saturate
    , desaturate = update2 0.5 (rgb 255 125 125) desaturate
    , rotateHue = update2 180 (rgb 255 0 0) desaturate
    , fadeIn = update2 0.3 (rgba 255 0 0 0.5) fadeIn
    , fadeOut = update2 0.3 (rgb 255 0 0) fadeOut
    , grayscale = update1 (rgb 255 0 0) Color.Manipulate.grayscale
    , scaleHsl = update2 ( 0.5, 0.5, 0.5 ) (rgb 255 0 0) Color.Manipulate.scaleHsl
    , mix = update2 (rgb 0 255 0) (rgb 255 0 0) Color.Manipulate.mix
    , weightedMix = update3 (rgb 0 255 0) (rgb 255 0 0) 0.5 weightedMix
    }


type Msg
    = Darken Float Color
    | Lighten Float Color
    | Saturate Float Color
    | Desaturate Float Color
    | RotateHue Float Color
    | FadeIn Float Color
    | FadeOut Float Color
    | Grayscale Color
    | ScaleHsl ( Float, Float, Float ) Color
    | Mix Color Color
    | WeightedMix Color Color Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        Darken v cl ->
            { model | darken = update2 v cl darken }

        Lighten v cl ->
            { model | lighten = update2 v cl lighten }

        Saturate v cl ->
            { model | saturate = update2 v cl saturate }

        Desaturate v cl ->
            { model | desaturate = update2 v cl desaturate }

        RotateHue v cl ->
            { model | rotateHue = update2 v cl rotateHue }

        FadeIn v cl ->
            { model | fadeIn = update2 v cl fadeIn }

        FadeOut v cl ->
            { model | fadeOut = update2 v cl fadeOut }

        Grayscale cl ->
            { model | grayscale = update1 cl Color.Manipulate.grayscale }

        ScaleHsl v cl ->
            { model | scaleHsl = update2 v cl scaleHsl }

        Mix v cl ->
            { model | mix = update2 v cl mix }

        WeightedMix cl1 cl2 v ->
            { model | weightedMix = update3 cl1 cl2 v weightedMix }


view : Model -> Html Msg
view model =
    div []
        [ (widget "darken" (args2 model.darken Darken F C))
        , (widget "lighten" (args2 model.lighten Lighten F C))
        , (widget "saturate" (args2 model.saturate Saturate F C))
        , (widget "desaturate" (args2 model.desaturate Desaturate F C))
        , (widget "rotateHue" (args2 model.rotateHue RotateHue F C))
        , (widget "fadeIn" (args2 model.fadeIn FadeIn F C))
        , (widget "fadeOut" (args2 model.fadeOut FadeOut F C))
        , (widget "grayscale" (args1 model.grayscale Grayscale C))
        , (widget "mix" (args2 model.mix Mix C C))
        , (widget "weightedMix" (args3 model.weightedMix WeightedMix C C F))
        ]


args3 ( a, b, c, d ) fn v1 v2 v3 =
    [ v1 ( a, \a_ -> fn a_ b c )
    , v2 ( b, \b_ -> fn a b_ c )
    , v3 ( c, \c_ -> fn a b c_ )
    , Result d
    ]


args2 ( a, b, c ) fn v1 v2 =
    [ v1 ( a, flip fn b )
    , v2 ( b, fn a )
    , Result c
    ]


args1 ( a, b ) fn v1 =
    [ v1 ( a, fn )
    , Result b
    ]


widget title args =
    div
        [ style
            [ ( "display", "flex" )
            , ( "marginBottom", "5px" )
            ]
        ]
        ((fnName title)
            :: functionDef args
        )


arrow : Html msg
arrow =
    span
        [ style [ ( "padding", "0 15px" ) ]
        ]
        [ text " -> " ]


swatch : Color -> List ( String, String ) -> Html msg
swatch cl s =
    div
        [ style
            ([ ( "background", colorToCssRgba cl )
             , ( "color", getTextColor cl )
             , ( "padding", "2px 4px" )
             , ( "height", "17px" )
             , ( "borderRadius", "3px" )
             , ( "fontFamily", "monospace" )
             ]
                ++ s
            )
        ]
        [ text (colorToHex cl) ]


fnName title =
    span
        [ style
            [ ( "fontFamily", "monospace" )
            , ( "width", "100px" )
            , ( "textAlign", "right" )
            , ( "paddingTop", "3px" )
            ]
        ]
        [ text (title ++ ":") ]


numberSelector v msg =
    input
        [ type_ "number"
        , Html.Attributes.step "0.01"
        , Html.Attributes.value (toString v)
        , onInput (String.toFloat >> Result.withDefault 0 >> \a -> msg a)
        , style
            [ ( "border", "none" )
            , ( "fontFamily", "monospace" )
            , ( "fontSize", "13px" )
            , ( "width", "44px" )
            , ( "textAlign", "right" )
            ]
        ]
        []


colorSelector cl msg =
    div
        [ style
            [ ( "position", "relative" )
            ]
        ]
        [ input
            [ type_ "color"
            , Html.Attributes.value (colorToHex cl)
            , onInput (hexToColor >> Result.withDefault darkGray >> \a -> msg a)
            , style
                [ ( "width", "58px" )
                , ( "height", "18px" )
                , ( "background", "none" )
                , ( "border", "none" )
                , ( "opacity", "0" )
                ]
            ]
            []
        , swatch cl
            [ ( "position", "absolute" )
            , ( "pointer-events", "none" )
            , ( "top", "0" )
            ]
        ]


getTextColor cl =
    let
        r1 =
            contrastRatio cl (rgb 0 0 0)

        r2 =
            contrastRatio cl (rgb 255 255 255)
    in
        if r1 < r2 then
            "#ffffff"
        else
            "#101010"


type Arg
    = F ( Float, Float -> Msg )
    | C ( Color, Color -> Msg )
    | Result Color


functionDef : List Arg -> List (Html Msg)
functionDef l =
    case l of
        [ x ] ->
            [ editable x ]

        x :: xs ->
            editable x :: arrow :: functionDef xs

        [] ->
            []


editable a =
    case a of
        F ( v, msg ) ->
            numberSelector v msg

        C ( v, msg ) ->
            colorSelector v msg

        Result v ->
            swatch v []
