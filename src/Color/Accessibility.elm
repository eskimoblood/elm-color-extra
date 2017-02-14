module Color.Accessibility exposing (contrastRatio, luminance, maximumContrast)

{-|
# Accessibility
Functions to measure and maximize accessibility.

@docs contrastRatio, luminance, maximumContrast
-}

import Color exposing (..)


{-|
Get the contrast ratio of two colors represented as a Float.

Formula based on:
https://www.w3.org/TR/WCAG20/#contrast-ratiodef

    contrastRatio Color.black Color.white -- 21.0
    contrastRatio Color.blue Color.blue -- 1.0
-}
contrastRatio : Color -> Color -> Float
contrastRatio c1 c2 =
    let
        a =
            (luminance c1) + 0.05

        b =
            (luminance c2) + 0.05
    in
        if a > b then
            a / b
        else
            b / a


{-|
Get the relative luminance of a color represented as a Float.

Formula based on:
https://www.w3.org/TR/WCAG20/#relativeluminancedef

    luminance Color.black -- 0.0
    luminance Color.white -- 1.0
-}
luminance : Color -> Float
luminance cl =
    let
        ( r, g, b ) =
            cl |> toRgb |> \a -> ( f a.red, f a.green, f a.blue )

        f intensity =
            let
                srgb =
                    (toFloat intensity) / 255
            in
                if srgb <= 0.03928 then
                    srgb / 12.92
                else
                    ((srgb + 0.055) / 1.055) ^ 2.4
    in
        0.2126 * r + 0.7152 * g + 0.0722 * b


{-|
Returns the color with the highest contrast to the base color.

    bgColor = Color.darkBlue
    textOptions = [ Color.white, Color.purple, Color.black ]

    maximumContrast bgColor textOptions -- Just Color.white
-}
maximumContrast : Color -> List Color -> Maybe Color
maximumContrast base options =
    let
        compareContrast c1 c2 =
            compare (contrastRatio base c2) (contrastRatio base c1)
    in
        options
            |> List.sortWith compareContrast
            |> List.head
