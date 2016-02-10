module Color.Convert (colorToCssRgb, colorToCssRgba, colorToCssHsl, colorToCssHsla, colorToHex, hexToColor, colorToLab, labToColor) where

{-|
#Convert
Convert colors to differnt string formats and hexadecimal strings to colors.

@docs colorToCssRgb, colorToCssRgba, colorToCssHsl, colorToCssHsla, colorToHex, hexToColor, colorToLab, labToColor
-}

import ParseInt exposing (parseIntHex)
import Color exposing (..)
import Regex
import Array
import String
import Result
import List
import Char
import String


{-|
Converts a color to an css rgb string.

    colorToCssRgb (rgb 255 0 0 ) -- "rgb(255, 0, 0)"
-}
colorToCssRgb : Color -> String
colorToCssRgb cl =
    let
        { red, green, blue, alpha } = toRgb cl
    in
        cssColorString "rgb" [ (toString red), (toString green), (toString blue) ]


{-|
Converts a color to an css rgba string.

    colorToCssRgb (rgba 255 0 0 0.5) -- "rgb(255, 0, 0, 0.5)"
-}
colorToCssRgba : Color -> String
colorToCssRgba cl =
    let
        { red, green, blue, alpha } = toRgb cl
    in
        cssColorString "rgba" [ (toString red), (toString green), (toString blue), (toString alpha) ]


{-|
Converts a color to an css hsl string.

    colorToCssRgb (hsl 1 1 0.5) -- "hsl(1, 1, 0.5)"
-}
colorToCssHsl : Color -> String
colorToCssHsl cl =
    let
        { hue, saturation, lightness, alpha } = toHsl cl
    in
        cssColorString "hsl" [ (hueToString hue), (toPercentString saturation), (toPercentString lightness) ]


{-|
Converts a color to an css hsla string.

    colorToCssRgb (hsla 1 1 0.5 1) -- "hsla(56, 100%, 50%, 1)"
-}
colorToCssHsla : Color -> String
colorToCssHsla cl =
    let
        { hue, saturation, lightness, alpha } = toHsl cl
    in
        cssColorString "hsla" [ (hueToString hue), (toPercentString saturation), (toPercentString lightness), (toString alpha) ]


hueToString : Float -> String
hueToString h =
    (h * 180 / pi) |> round |> toString


toPercentString : Float -> String
toPercentString h =
    ((h * 100) |> round |> toString) ++ "%"


cssColorString : String -> List String -> String
cssColorString kind values =
    kind
        ++ "("
        ++ (String.join ", " values)
        ++ ")"


{-|
Converts a string to `Maybe` of color.

    hexToColor "#ff0000" -- "Just RGB 255 0 0"
    hexToColor "ff0000" -- "Just RGB 255 0 0"
    hexToColor "1234" -- "Nothing"

-}
hexToColor : String -> Maybe Color
hexToColor c =
    let
        r =
            Regex.find Regex.All (Regex.regex "^#?([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2})$") (String.toLower c)
                |> List.map .submatches
                |> List.head
    in
        case r of
            Just sm ->
                let
                    v =
                        List.filterMap identity sm
                            |> List.map parseIntHex
                            |> List.map Result.toMaybe
                            |> List.filterMap identity
                            |> Array.fromList

                    r = Array.get 0 v

                    g = Array.get 1 v

                    b = Array.get 2 v
                in
                    case r of
                        Just r' ->
                            case g of
                                Just g' ->
                                    case b of
                                        Just b' ->
                                            Just (rgb r' g' b')

                                        Nothing ->
                                            Nothing

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing

            Nothing ->
                Nothing


{-|
Converts a color to a hexadecimal string.

    hexToColor (rgb 255 0 0) -- "#ff0000"

-}
colorToHex : Color -> String
colorToHex cl =
    let
        { red, green, blue, alpha } = toRgb cl
    in
        "#" ++ (toHex red) ++ (toHex green) ++ (toHex blue)


toHex : Int -> String
toHex n =
    let
        hex = toRadix n
    in
        if String.length hex == 1 then
            "0" ++ hex
        else
            hex


toRadix : Int -> String
toRadix n =
    let
        getChr c =
            if c < 10 then
                toString c
            else
                String.fromChar <| Char.fromCode (87 + c)
    in
        if n < 16 then
            getChr n
        else
            (toRadix (n // 16)) ++ (getChr (n % 16))


{-| Convert color to CIELAB- color space
-}
colorToLab : Color -> { l : Float, a : Float, b : Float }
colorToLab cl =
    let
        { red, green, blue } = toRgb cl

        r = c red

        g = c green

        b = c blue

        x = l ((r * 0.4124 + g * 0.3576 + b * 0.1805) / 95.047)

        y = l ((r * 0.2126 + g * 0.7152 + b * 7.22e-2) / 100)

        z = l ((r * 1.93e-2 + g * 0.1192 + b * 0.9505) / 108.883)
    in
        { l = (116 * y) - 16
        , a = 500 * (x - y)
        , b = 200 * (y - z)
        }


c : Int -> Float
c ch =
    let
        ch' = (toFloat ch) / 255

        ch'' =
            if ch' > 4.045e-2 then
                ((ch' + 5.5e-2) / 1.055) ^ 2.4
            else
                ch' / 12.92
    in
        ch'' * 100


l : Float -> Float
l ch =
    if ch > 8.856e-3 then
        ch ^ (1 / 3)
    else
        (7.787 * ch) + (16 / 116)

{-| Convert a color in CIELAB- color space to Elm `Color`
-}
labToColor : { l : Float, a : Float, b : Float } -> Color
labToColor { l, a, b } =
    let
        y = (d ((l + 16) / 116))

        x = (d (a / 500 + y)) * 95.047 / 100

        z = (d (y - b / 200)) * 108.883 / 100

        r = x * 3.2406 + y * -1.5372 + z * -0.4986

        g = x * -0.9689 + y * 1.8758 + z * 4.15e-2

        b = x * 5.57e-2 + y * -0.204 + z * 1.057
    in
        rgb
            (round (f r) * 255)
            (round (f g) * 255)
            (round (f b) * 255)


d : Float -> Float
d ch =
    if ch ^ 3 > 8.856e-3 then
        ch ^ 3
    else
        (ch - 16 / 116) / 7.787


f : Float -> Float
f ch =
    if ch > 3.1308e-3 then
        1.055 * (ch ^ (1 / 2.4)) - 5.5e-2
    else
        12.92 * ch
