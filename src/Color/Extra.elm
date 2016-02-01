module Color.Extra (blend, darken, lighten, saturate, desaturate, rotateHue, gradient, toCssString, Palette, GradientStop, Gradient) where

{-| A library for creating and manipulating colors.


# Color adjustment
@docs darken, lighten, saturate, desaturate, rotateHue

# Mixing colors
@docs blend

# Gradient
@docs GradientStop, Gradient, Palette, gradient

#Helper
@docs toCssString
-}

import Color exposing (Color, toHsl, hsla, toRgb, rgba)
import Maybe exposing (..)
import String


{-| -}
type alias Palette =
    List Color


{-| A color and a stop value that indicates where the color appears in a gradient.
 The stop value must be between `0` and `1`.
-}
type alias GradientStop =
    ( Float, Color )


{-| -}
type alias Gradient =
    List GradientStop


{-| convert a color into an CSS rgba color string
-}
toCssString : Color -> String
toCssString cl =
    let
        { red, green, blue, alpha } = toRgb cl

        rgba =
            [ (toFloat red), (toFloat green), (toFloat blue), alpha ]
                |> List.map toString
                |> (String.join ",")
    in
        "rgba(" ++ rgba ++ ")"


interpolate : Float -> Float -> Float -> Float
interpolate t i1 i2 =
    i1 + (i2 - i1) * t


limit : Float -> Float
limit =
    clamp 0 1


{-| Linear interpolation of  two colors.
-}
blend : Color -> Color -> Float -> Color
blend c1 c2 t =
    let
        c1' = toRgb c1

        c2' = toRgb c2

        i = interpolate t
    in
        rgba
            (round (i (toFloat c1'.red) (toFloat c2'.red)))
            (round (i (toFloat c1'.green) (toFloat c2'.green)))
            (round (i (toFloat c1'.blue) (toFloat c2'.blue)))
            (i c1'.alpha c2'.alpha)


{-| Decrease the lightning of a colosds
-}
darken : Float -> Color -> Color
darken offset cl =
    let
        { hue, saturation, lightness, alpha } = toHsl cl
    in
        hsla hue saturation (limit (lightness + offset)) alpha


{-| Increase the lightning of a color
-}
lighten : Float -> Color -> Color
lighten offset cl =
    darken -offset cl


{-| Increase the saturation of a color
-}
saturate : Float -> Color -> Color
saturate offset cl =
    let
        { hue, saturation, lightness, alpha } = toHsl cl
    in
        hsla hue (limit (saturation + offset)) lightness alpha


{-| Decrease the saturation of a color
-}
desaturate : Float -> Color -> Color
desaturate offset cl =
    saturate -offset cl


{-| Convert the color to a greyscale version, aka set saturation to 0
-}
greyscale : Color -> Color
greyscale cl =
    saturate -1 cl


{-| Increase the opacity of a color
-}
fadeIn : Float -> Color -> Color
fadeIn offset cl =
    let
        { hue, saturation, lightness, alpha } = toHsl cl
    in
        hsla hue saturation lightness (limit (alpha + offset))


{-| Decrease the opacity of a color
-}
fadeOut : Float -> Color -> Color
fadeOut offset cl =
    fadeIn -offset cl


{-| Change the hue of a color. The angle value must be in degrees
-}
rotateHue : Float -> Color -> Color
rotateHue angle cl =
    let
        { hue, saturation, lightness, alpha } = toHsl cl
    in
        hsla (hue + (degrees angle)) saturation lightness alpha


{-| Create a new `Palette` from a given `Gradient`, with a given size.
-}
gradient : Gradient -> Int -> Palette
gradient stops size =
    let
        purifiedStops =
            stops
                |> List.filter (\( t, _ ) -> t >= 0 && t <= 1)
                |> List.sortBy (\( t, _ ) -> t)

        stop1 = List.head purifiedStops
    in
        case stop1 of
            Just s1 ->
                let
                    l = size - 1

                    stops = [0..l] |> List.map (\i -> (toFloat i) / l)

                    currentStops = Maybe.withDefault [] (List.tail purifiedStops)

                    ( s2, g ) = getNextGradientStop s1 currentStops
                in
                    List.foldl c ( s1, s2, g, [] ) stops
                        |> (\( _, _, _, p ) -> p)
                        |> List.reverse

            Nothing ->
                []


c : Float -> ( GradientStop, GradientStop, Gradient, Palette ) -> ( GradientStop, GradientStop, Gradient, Palette )
c t ( stop1, stop2, gradient, palette ) =
    let
        ( stop1', stop2', gradient', color ) = calculateGradient stop1 stop2 gradient t
    in
        ( stop1', stop2', gradient, (color :: palette) )


calculateGradient : GradientStop -> GradientStop -> Gradient -> Float -> ( GradientStop, GradientStop, Gradient, Color )
calculateGradient stop1 stop2 gradient t =
    if (fst stop2 < t) then
        let
            stop1' = stop2

            ( stop2', gradient' ) = getNextGradientStop stop2 gradient
        in
            ( stop1', stop2', gradient', (calculateColor stop1' stop2' t) )
    else
        ( stop1, stop2, gradient, (calculateColor stop1 stop2 t) )


calculateColor : GradientStop -> GradientStop -> Float -> Color
calculateColor ( t1, cl1 ) ( t2, cl2 ) t =
    if t == 0 then
        cl1
    else if t == 1 then
        cl2
    else
        blend cl1 cl2 ((t - t1) / (t2 - t1))


getNextGradientStop : GradientStop -> Gradient -> ( GradientStop, Gradient )
getNextGradientStop currentStop gradient =
    let
        nextStop = List.head gradient
    in
        case nextStop of
            Just s ->
                ( s, Maybe.withDefault [] (List.tail gradient) )

            Nothing ->
                ( currentStop, gradient )
