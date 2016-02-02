module Color.Convert (colorToCssRgb, colorToCssRgba, colorToCssHsl, colorToCssHsla, colorToHex, hexToColor) where

{-|
#Convert
Convert colors to differnt string formats and hexadecimal strings to colors.

@docs colorToCssRgb, colorToCssRgba, colorToCssHsl, colorToCssHsla, colorToHex, hexToColor
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
        cssColorString "rgb" [ (toFloat red), (toFloat green), (toFloat blue) ]


{-|
Converts a color to an css rgba string.

    colorToCssRgb (rgba 255 0 0 0.5) -- "rgb(255, 0, 0, 0.5)"
-}
colorToCssRgba : Color -> String
colorToCssRgba cl =
    let
        { red, green, blue, alpha } = toRgb cl
    in
        cssColorString "rgba" [ (toFloat red), (toFloat green), (toFloat blue), alpha ]


{-|
Converts a color to an css hsl string.

    colorToCssRgb (hsl 1 1 0.5) -- "hsl(1, 1, 0.5)"
-}
colorToCssHsl : Color -> String
colorToCssHsl cl =
    let
        { hue, saturation, lightness, alpha } = toHsl cl
    in
        cssColorString "hsl" [ hue, saturation, lightness ]


{-|
Converts a color to an css hsla string.

    colorToCssRgb (hsla 1 1 0.5 1) -- "hsla(1, 1, 0.5, 1)"
-}
colorToCssHsla : Color -> String
colorToCssHsla cl =
    let
        { hue, saturation, lightness, alpha } = toHsl cl
    in
        cssColorString "hsla" [ hue, saturation, lightness, alpha ]


cssColorString : String -> List Float -> String
cssColorString kind values =
    kind
        ++ "("
        ++ (List.map toString values
                |> String.join ", "
           )
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
