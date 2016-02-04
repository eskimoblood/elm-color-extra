module Color.Blending (mix, multiply, screen, overlay, difference, exclusion, hardlight, softlight, colorBurn, colorDodge, lighten, darken) where

{-|
# Blending
Based on the [Compositing and Blending Level 1](https://www.w3.org/TR/compositing-1/#blending)

@docs mix, multiply, screen, overlay, difference, exclusion, hardlight, softlight, colorBurn, colorDodge, lighten, darken
-}

import Color exposing (Color, toRgb, rgba)


{-| Linear interpolation of two colors by a factor between `0` and `1`.
-}
mix : Color -> Color -> Float -> Color
mix cB cS t =
    let
        cB' = toRgb cB

        cS' = toRgb cS

        i = interpolate t
    in
        rgba
            (round (i (toFloat cB'.red) (toFloat cS'.red)))
            (round (i (toFloat cB'.green) (toFloat cS'.green)))
            (round (i (toFloat cB'.blue) (toFloat cS'.blue)))
            (i cB'.alpha cS'.alpha)


interpolate : Float -> Float -> Float -> Float
interpolate t i1 i2 =
    i1 + (i2 - i1) * t


{-|
The source color is multiplied by the destination color and replaces the destination.

The resultant color is always at least as dark as either the source or destination color.
Multiplying any color with black results in black.
Multiplying any color with white preserves the original color.
-}
multiply : Color -> Color -> Color
multiply clB clS =
    colorBlend (*) clB clS


{-|
Multiplies the complements of the backdrop and source color values,
 then complements the result.
-}
screen : Color -> Color -> Color
screen clB clS =
    colorBlend screen' clB clS


{-|
Multiplies or screens the colors, depending on the backdrop color value.
-}
overlay : Color -> Color -> Color
overlay clB clS =
    colorBlend overlay' clB clS


{-|
Selects the darker of the backdrop and source colors.
-}
darken : Color -> Color -> Color
darken clB clS =
    colorBlend min clB clS


{-|
Selects the lighter of the backdrop and source colors.
-}
lighten : Color -> Color -> Color
lighten clB clS =
    colorBlend max clB clS


{-|
Subtracts the darker of the two constituent colors from the lighter color.
-}
difference : Color -> Color -> Color
difference clB clS =
    colorBlend (\cB cS -> abs (cB - cS)) clB clS


{-|
Produces an effect similar to that of the Difference mode but lower in contrast.
Painting with white inverts the backdrop color; painting with black produces no change
-}
exclusion : Color -> Color -> Color
exclusion clB clS =
    colorBlend (\cB cS -> cB + cS - 2 * cB * cS) clB clS


{-|
Multiplies or screens the colors, depending on the source color value.
The effect is similar to shining a harsh spotlight on the backdrop.
-}
hardlight : Color -> Color -> Color
hardlight clB clS =
    overlay clS clB


{-|
Darkens or lightens the colors, depending on the source color value.
The effect is similar to shining a diffused spotlight on the backdrop.
-}
softlight : Color -> Color -> Color
softlight clB clS =
    colorBlend softlight' clB clS


{-|
Darkens the backdrop color to reflect the source color.
Painting with white produces no change.
-}
colorBurn : Color -> Color -> Color
colorBurn clB clS =
    colorBlend colorBurn' clB clS


{-|
Brightens the backdrop color to reflect the source color.
Painting with black produces no changes.
-}
colorDodge : Color -> Color -> Color
colorDodge clB clS =
    colorBlend colorDodge' clB clS


colorBlend : (Float -> Float -> Float) -> Color -> Color -> Color
colorBlend fn clB clS =
    let
        rgba1 = toRgb clB

        rgba2 = toRgb clS

        ar = rgba2.alpha + rgba1.alpha * (1 - rgba2.alpha)

        calc = calcChanel fn rgba1.alpha rgba2.alpha ar
    in
        rgba
            (calc rgba1.red rgba2.red)
            (calc rgba1.green rgba2.green)
            (calc rgba1.blue rgba2.blue)
            ar


calcChanel : (Float -> Float -> Float) -> Float -> Float -> Float -> Int -> Int -> Int
calcChanel fn aB aS ar cB cS =
    let
        cB' = toFloat cB / 255

        cS' = toFloat cS / 255

        cr = fn cB' cS'

        cr' =
            if ar == 0 then
                cr
            else
                (aS * cS' + aB * (cB' - aS * (cB' + cS' - cr))) / ar
    in
        round (clampChannel cr' * 255)


clampChannel : number -> number
clampChannel =
    clamp 0 1


screen' : Float -> Float -> Float
screen' cB cS =
    cB + cS - cB * cS


overlay' : Float -> Float -> Float
overlay' cB cS =
    let
        cB' = cB * 2
    in
        if (cB' <= 1) then
            cB' * cS
        else
            screen' (cB' - 1) cS


softlight' : Float -> Float -> Float
softlight' cB cS =
    let
        ( d, e ) =
            if (cS > 0.5) then
                if (cB > 0.25) then
                    ( sqrt cB, 1 )
                else
                    ( ((16 * cB - 12) * cB + 4) * cB, 1 )
            else
                ( 1, cB )
    in
        cB - (1 - 2 * cS) * e * (d - cB)


colorBurn' : Float -> Float -> Float
colorBurn' cB cS =
    if cB == 1 then
        1
    else if cS == 0 then
        0
    else
        1 - min 1 (1 - cB) / cS


colorDodge' : Float -> Float -> Float
colorDodge' cB cS =
    if cB == 0 then
        0
    else if cS == 1 then
        1
    else
        min 1 cB / (1 - cS)
