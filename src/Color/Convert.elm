module Color.Convert exposing (colorToCssRgb, colorToCssRgba, colorToCssHsl, colorToCssHsla, colorToHex, hexToColor, colorToLab, labToColor)

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


type alias XYZ =
    { x : Float, y : Float, z : Float }


type alias Lab =
    { l : Float, a : Float, b : Float }


{-|
Converts a color to an css rgb string.

    colorToCssRgb (rgb 255 0 0) -- "rgb(255, 0, 0)"
-}
colorToCssRgb : Color -> String
colorToCssRgb cl =
    let
        { red, green, blue, alpha } =
            toRgb cl
    in
        cssColorString "rgb" [ (toString red), (toString green), (toString blue) ]


{-|
Converts a color to an css rgba string.

    colorToCssRgba (rgba 255 0 0 0.5) -- "rgba(255, 0, 0, 0.5)"
-}
colorToCssRgba : Color -> String
colorToCssRgba cl =
    let
        { red, green, blue, alpha } =
            toRgb cl
    in
        cssColorString "rgba" [ (toString red), (toString green), (toString blue), (toString alpha) ]


{-|
Converts a color to an css hsl string.

    colorToCssHsl (hsl 1 1 0.5) -- "hsl(1, 1, 0.5)"
-}
colorToCssHsl : Color -> String
colorToCssHsl cl =
    let
        { hue, saturation, lightness, alpha } =
            toHsl cl
    in
        cssColorString "hsl" [ (hueToString hue), (toPercentString saturation), (toPercentString lightness) ]


{-|
Converts a color to an css hsla string.

    colorToCssHsla (hsla 1 1 0.5 1) -- "hsla(56, 100%, 50%, 1)"
-}
colorToCssHsla : Color -> String
colorToCssHsla cl =
    let
        { hue, saturation, lightness, alpha } =
            toHsl cl
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
                in
                    case v of
                        r::g::b::[] ->
                            Just (rgb r g b)
                        _ ->
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
        { red, green, blue, alpha } =
            toRgb cl
    in
        "#" ++ (toHex red) ++ (toHex green) ++ (toHex blue)


toHex : Int -> String
toHex n =
    let
        hex =
            toRadix n
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
    colorToXyz cl |> xyzToLab


colorToXyz : Color -> XYZ
colorToXyz cl =
    let
        c ch =
            let
                ch' =
                    (toFloat ch) / 255

                ch'' =
                    if ch' > 4.045e-2 then
                        ((ch' + 5.5e-2) / 1.055) ^ 2.4
                    else
                        ch' / 12.92
            in
                ch'' * 100

        { red, green, blue } =
            toRgb cl

        r =
            c red

        g =
            c green

        b =
            c blue
    in
        { x = r * 0.4124 + g * 0.3576 + b * 0.1805
        , y = r * 0.2126 + g * 0.7152 + b * 7.22e-2
        , z = r * 1.93e-2 + g * 0.1192 + b * 0.9505
        }


xyzToLab : XYZ -> Lab
xyzToLab { x, y, z } =
    let
        c ch =
            if ch > 8.856e-3 then
                ch ^ (1 / 3)
            else
                (7.787 * ch) + (16 / 116)

        x' =
            c (x / 95.047)

        y' =
            c (y / 100)

        z' =
            c (z / 108.883)
    in
        { l = (116 * y') - 16
        , a = 500 * (x' - y')
        , b = 200 * (y' - z')
        }


{-| Convert a color in CIELAB- color space to Elm `Color`
-}
labToColor : { l : Float, a : Float, b : Float } -> Color
labToColor lab =
    labToXyz lab |> xyzToColor


labToXyz : Lab -> XYZ
labToXyz { l, a, b } =
    let
        c ch =
            let
                ch' =
                    ch * ch * ch
            in
                if ch' > 8.856e-3 then
                    ch'
                else
                    (ch - 16 / 116) / 7.787

        y =
            (l + 16) / 116
    in
        { y = (c y) * 100
        , x = (c (y + a / 500)) * 95.047
        , z = (c (y - b / 200)) * 108.883
        }


xyzToColor : XYZ -> Color
xyzToColor { x, y, z } =
    let
        x' =
            x / 100

        y' =
            y / 100

        z' =
            z / 100

        r = x' * 3.2404542 + y' * -1.5371385 + z' * -0.4986
        g = x' * -0.969266 + y' * 1.8760108 + z' * 4.1556e-2
        b = x' * 5.56434e-2 + y' * -0.2040259 + z' * 1.0572252

        c ch =
            let
                ch' =
                  if ch > 3.1308e-3 then
                      1.055 * (ch ^ (1 / 2.4)) - 5.5e-2
                  else
                      12.92 * ch
             in
                 round <| clamp 0 255 (ch' * 255)

    in
        rgb (c r) (c g) (c b)
