module Tests (..) where

import ElmTest exposing (..)
import String
import Color.Convert exposing (..)
import Color.Manipulate exposing (..)
import Color exposing (rgb, rgba, hsl, hsla)


convert : Test
convert =
    suite
        "Convert"
        [ test "Color to rgb String" (assertEqual (colorToCssRgb (rgb 255 125 0)) "rgb(255, 125, 0)")
        , test "Color to rgba String" (assertEqual (colorToCssRgba (rgba 255 125 0 0.3)) "rgba(255, 125, 0, 0.3)")
        , test "Color to hsl String" (assertEqual (colorToCssHsl (hsl 0.4 0.2 0)) "hsl(0.4, 0.2, 0)")
        , test "Color to hsla String" (assertEqual (colorToCssHsla (hsla 0.4 0.2 0 1)) "hsla(0.4, 0.2, 0, 1)")
        , test "Color to hex String" (assertEqual (colorToHex (rgb 255 0 255)) "#ff00ff")
        , test "Hex string to hex color" (assertEqual (hexToColor "#ff00ff") (Just (rgb 255 0 255)))
        , test "Hex string to hex color" (assertEqual (hexToColor "ff00ff") (Just (rgb 255 0 255)))
        , test "Hex string to hex color" (assertEqual (hexToColor "1234") Nothing)
        ]


manipulate : Test
manipulate =
    suite
        "Convert"
        [ test "Darken" (assertEqual (darken 0.5 (hsl 1 1 0.2)) (hsl 1 1 0.7))
        , test "Darken should be limit to 1" (assertEqual (darken 10 (hsl 1 1 1)) (hsl 1 1 1))
        , test "Lighten" (assertEqual (lighten 0.5 (hsl 1 1 1)) (hsl 1 1 0.5))
        , test "Lighten should be limit to 0" (assertEqual (lighten 10 (hsl 1 1 1)) (hsl 1 1 0))
        , test "Saturate" (assertEqual (saturate 0.5 (hsl 1 0 1)) (hsl 1 0.5 1))
        , test "Saturate should be limit to 1" (assertEqual (saturate 10 (hsl 1 1 1)) (hsl 1 1 1))
        , test "Desaturate" (assertEqual (desaturate 0.5 (hsl 1 1 1)) (hsl 1 0.5 1))
        , test "Desaturate should be limit to 0" (assertEqual (desaturate 10 (hsl 1 1 1)) (hsl 1 0 1))
        , test "Grayscale" (assertEqual (Color.Manipulate.grayscale (hsl 1 1 1)) (hsl 1 0 1))
        , test "Fade in" (assertEqual (fadeIn 0.2 (hsla 1 1 1 0.5)) (hsla 1 1 1 0.7))
        , test "Fade in should be limit to 1" (assertEqual (fadeIn 10 (hsla 1 1 1 0.5)) (hsla 1 1 1 1))
        , test "Fade out" (assertEqual (fadeOut 0.2 (hsla 1 1 1 0.5)) (hsla 1 1 1 0.3))
        , test "Fade out should be limit to 0" (assertEqual (fadeOut 10 (hsla 1 1 1 0.5)) (hsla 1 1 1 0))
        ]


all : Test
all =
    suite
        "All tests"
        [ convert
        , manipulate
        ]
