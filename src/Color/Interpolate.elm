module Color.Interpolate (Space(RGB, HSL), interpolate) where

{-|
# Interpolate
Interpolate between two colors

@docs Space, interpolate
-}

import Color exposing (Color, toRgb, rgba, toHsl, hsla)
import Color.Convert exposing (labToColor, colorToLab)


{-| The color space that is used for the interpolation
-}
type Space
    = RGB
    | HSL
    | LAB


degree180 : Float
degree180 =
    degrees 180


degree360 : Float
degree360 =
    degrees 360


{-| Linear interpolation of two colors by a factor between `0` and `1`.
-}
interpolate : Space -> Color -> Color -> Float -> Color
interpolate space cl1 cl2 t =
    let
        i = linear t
    in
        case space of
            RGB ->
                let
                    cl1' = toRgb cl1

                    cl2' = toRgb cl2
                in
                    rgba
                        (round (i (toFloat cl1'.red) (toFloat cl2'.red)))
                        (round (i (toFloat cl1'.green) (toFloat cl2'.green)))
                        (round (i (toFloat cl1'.blue) (toFloat cl2'.blue)))
                        (i cl1'.alpha cl2'.alpha)

            HSL ->
                let
                    cl1' = toHsl cl1

                    cl2' = toHsl cl2

                    h1 = cl1'.hue

                    h2 = cl2'.hue

                    dH =
                        if h2 > h1 && h2 - h1 > degree180 then
                            h2 - h1 + degree360
                        else if h2 < h1 && h1 - h2 > degree180 then
                            h2 + degree360 - h1
                        else
                            h2 - h1

                    hue = h1 + t * dH
                in
                    hsla
                        hue
                        (i cl1'.saturation cl2'.saturation)
                        (i cl1'.lightness cl2'.lightness)
                        (i cl1'.alpha cl2'.alpha)

            LAB ->
                let
                    lab1 = colorToLab cl1

                    lab2 = colorToLab cl2

                    l = i lab1.l lab2.l

                    a = i lab1.a lab2.a

                    b = i lab1.b lab2.b
                in
                    labToColor { l = l, a = a, b = b }


linear : Float -> Float -> Float -> Float
linear t i1 i2 =
    i1 + (i2 - i1) * t
