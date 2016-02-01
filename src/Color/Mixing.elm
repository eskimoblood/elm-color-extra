module Mixing.Elm ( multiply, screen, overlay, difference, exclusion, average, negation, hardlight, softlight) where

import Color exposing (Color, toRgb, rgba)


multiply : Color -> Color -> Color
multiply cl1 cl2 =
    colorBlend (*) cl1 cl2


screen : Color -> Color -> Color
screen cl1 cl2 =
    colorBlend screen' cl1 cl2


overlay : Color -> Color -> Color
overlay cl1 cl2 =
    colorBlend overlay' cl1 cl2


difference : Color -> Color -> Color
difference cl1 cl2 =
    colorBlend (\c1 c2 -> abs (c1 - c2)) cl1 cl2


exclusion : Color -> Color -> Color
exclusion cl1 cl2 =
    colorBlend (\c1 c2 -> c1 + c2 - 2 * c1 * c2) cl1 cl2


average : Color -> Color -> Color
average cl1 cl2 =
    colorBlend (\c1 c2 -> (c1 + c2) / 2) cl1 cl2


negation : Color -> Color -> Color
negation cl1 cl2 =
    colorBlend (\c1 c2 -> 1 - abs (c1 + c2 - 1)) cl1 cl2


hardlight : Color -> Color -> Color
hardlight cl1 cl2 =
    overlay cl2 cl1


softlight : Color -> Color -> Color
softlight cl1 cl2 =
    colorBlend softlight' cl1 cl2


colorBlend : (Float -> Float -> Float) -> Color -> Color -> Color
colorBlend fn cl1 cl2 =
    let
        rgba1 = toRgb cl1

        rgba2 = toRgb cl2

        ar = rgba2.alpha + rgba1.alpha * (1 - rgba2.alpha)

        calc = calcChanel fn rgba1.alpha rgba2.alpha ar
    in
        rgba
            (calc rgba1.red rgba2.red)
            (calc rgba1.green rgba2.green)
            (calc rgba1.blue rgba2.blue)
            ar


calcChanel : (Float -> Float -> Float) -> Float -> Float -> Float -> Int -> Int -> Int
calcChanel fn a1 a2 ar c1 c2 =
    let
        c1' = toFloat c1 / 255

        c2' = toFloat c2 / 255

        cr = fn c1' c2'

        cr' =
            if ar == 0 then
                cr
            else
                (a2 * c2' + a1 * (c1' - a2 * (c1' + c2' - cr))) / ar
    in
        round (cr' * 255)


screen' : Float -> Float -> Float
screen' c1 c2 =
    c1 + c2 - c1 * c1


overlay' : Float -> Float -> Float
overlay' c1 c2 =
    let
        c1' = c1 * 2
    in
        if (c1' <= 1) then
            c1' * c2
        else
            screen' (c1' - 1) c2


softlight' : Float -> Float -> Float
softlight' c1 c2 =
    let
        ( d, e ) =
            if (c2 > 0.5) then
                if (c1 > 0.25) then
                    ( sqrt c1, 1 )
                else
                    ( ((16 * c1 - 12) * c1 + 4) * c1, 1 )
            else
                ( 1, c1 )
    in
        c1 - (1 - 2 * c2) * e * (d - c1)
