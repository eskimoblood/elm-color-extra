module Tests exposing (..)

import Test exposing (..)
import Expect
import Color.Accessibility exposing (..)
import Color.Convert exposing (..)
import Color.Gradient as Gra exposing (..)
import Color.Manipulate as Man exposing (..)
import Color.Blending as Ble exposing (..)
import Color.Interpolate as Int exposing (..)
import Color exposing (Color, rgb, rgba, hsl, hsla)


accessibility : Test
accessibility =
    describe "Accessibility"
        [ test "Contrast ratio of black and white should be 21:1" <|
            \() ->
                Expect.equal
                    (contrastRatio Color.black Color.white)
                    21.0
        , test "Contrast ratio of equal colors should be 1:1" <|
            \() ->
                Expect.equal
                    (contrastRatio Color.blue Color.blue)
                    1.0
        , test "Contrast ratio color order does not matter" <|
            \() ->
                Expect.equal
                    (contrastRatio Color.green Color.blue)
                    (contrastRatio Color.blue Color.green)
        , test "Luminance of black is the minimum possible" <|
            \() ->
                Expect.equal
                    (luminance Color.black)
                    0.0
        , test "Luminance of white is the maximum possible" <|
            \() ->
                Expect.equal
                    (luminance Color.white)
                    1.0
        , test "Maximum contrast" <|
            \() ->
                Expect.equal
                    (maximumContrast Color.yellow
                        [ Color.white
                        , Color.darkBlue
                        , Color.green
                        ]
                    )
                    (Just Color.darkBlue)
        ]


convert : Test
convert =
    describe "Convert"
        [ test "Color to rgb String" <|
            \() -> (Expect.equal (colorToCssRgb (rgb 255 125 0)) "rgb(255, 125, 0)")
        , test "Color to rgba String" <|
            \() -> (Expect.equal (colorToCssRgba (rgba 255 125 0 0.3)) "rgba(255, 125, 0, 0.3)")
        , test "Color to hsl String" <|
            \() -> (Expect.equal (colorToCssHsl (hsl 0.4 0.2 0)) "hsl(23, 20%, 0%)")
        , test "Color to hsla String" <|
            \() -> (Expect.equal (colorToCssHsla (hsla 0.4 0.2 0 1)) "hsla(23, 20%, 0%, 1)")
        , test "Color to hex String" <|
            \() -> (Expect.equal (colorToHex (rgb 255 0 255)) "#ff00ff")
        , test "Hex string to hex color" <|
            \() -> Expect.equal (hexToColor "#ff00ff") (Ok <| rgb 255 0 255)
        , test "Hex string to hex color" <|
            \() -> Expect.equal (hexToColor "ff00ff") (Ok <| rgb 255 0 255)
        , test "Hex string to hex color" <|
            \() -> Expect.equal (hexToColor "#f0f") (Ok <| rgb 255 0 255)
        , test "Hex string to hex color" <|
            \() -> Expect.equal (hexToColor "0a0") (Ok <| rgb 0 170 0)
        , test "Hex string to hex color" <|
            \() -> Expect.equal (hexToColor "1234") (Err "Parsing hex regex failed")
        , test "Rgb to lab" <|
            \() -> (Expect.equal lab1 (colorToLab (rgb 255 255 0)))
        , test "Lab to rgb" <|
            \() -> (Expect.equal (rgb 255 255 0) (labToColor lab1))
        ]


lab1 : { l : Float, a : Float, b : Float }
lab1 =
    { l = 97.13824698129729, a = -21.555908334832285, b = 94.48248544644461 }


manipulate : Test
manipulate =
    describe "Manipulate"
        [ test "Darken" <|
            \() -> (Expect.equal (Man.darken 0.5 (hsl 1 1 1)) (hsl 1 1 0.5))
        , test "Darken should be limit to 0" <|
            \() -> (Expect.equal (Man.darken 10 (hsl 1 1 1)) (hsl 1 1 0))
        , test "Lighten" <|
            \() -> (Expect.equal (Man.lighten 0.5 (hsl 1 1 0.2)) (hsl 1 1 0.7))
        , test "Lighten should be limit to 1" <|
            \() -> (Expect.equal (Man.lighten 10 (hsl 1 1 0)) (hsl 1 1 1))
        , test "Saturate" <|
            \() -> (Expect.equal (saturate 0.5 (hsl 1 0 1)) (hsl 1 0.5 1))
        , test "Saturate should be limit to 1" <|
            \() -> (Expect.equal (saturate 10 (hsl 1 1 1)) (hsl 1 1 1))
        , test "Desaturate" <|
            \() -> (Expect.equal (desaturate 0.5 (hsl 1 1 1)) (hsl 1 0.5 1))
        , test "Desaturate should be limit to 0" <|
            \() -> (Expect.equal (desaturate 10 (hsl 1 1 1)) (hsl 1 0 1))
        , test "Grayscale" <|
            \() -> (Expect.equal (Man.grayscale (hsl 1 1 1)) (hsl 1 0 1))
        , test "Fade in" <|
            \() -> (Expect.equal (fadeIn 0.2 (hsla 1 1 1 0.5)) (hsla 1 1 1 0.7))
        , test "Fade in should be limit to 1" <|
            \() -> (Expect.equal (fadeIn 10 (hsla 1 1 1 0.5)) (hsla 1 1 1 1))
        , test "Fade out" <|
            \() -> (Expect.equal (fadeOut 0.2 (hsla 1 1 1 0.5)) (hsla 1 1 1 0.3))
        , test "Fade out should be limit to 0" <|
            \() -> (Expect.equal (fadeOut 10 (hsla 1 1 1 0.5)) (hsla 1 1 1 0))
        , test "Rotate hue" <|
            \() -> (Expect.equal (rotateHue 90 (hsla 0 1 1 0)) (hsla (degrees 90) 1 1 0))
        , test "Rotate hue with negative value" <|
            \() -> (Expect.equal (rotateHue -90 (hsla 0 1 1 0)) (hsla (degrees 270) 1 1 0))
        , test "Rotate hue for more then 360°" <| \() -> (Expect.equal (rotateHue 270 (hsla (degrees 180) 1 1 0)) (hsla (degrees 90) 1 1 0))
        , test "Scale saturation with positive value" <| \() -> (Expect.equal (hsl (degrees 120) 0.51 0.9) (scaleHsl ( 0.3, 0, 0 ) (hsl (degrees 120) 0.3 0.9)))
        , test "Scale saturation with negative value" <| \() -> (Expect.equal (hsl (degrees 120) 0.21 0.9) (scaleHsl ( -0.3, 0, 0 ) (hsl (degrees 120) 0.3 0.9)))
        , test "Scale lightness with positive value" <| \() -> (Expect.equal (hsl (degrees 120) 0.3 0.915) (scaleHsl ( 0, 0.15, 0 ) (hsl (degrees 120) 0.3 0.9)))
        , test "Scale lightness with negative value" <| \() -> (Expect.equal (hsl (degrees 120) 0.3 0.765) (scaleHsl ( 0, -0.15, 0 ) (hsl (degrees 120) 0.3 0.9)))
        , test "Scale alpha with positive value" <| \() -> (Expect.equal (hsla (degrees 120) 0.3 0.9 0.14) (scaleHsl ( 0, 0, 0.14 ) (hsla (degrees 120) 0.3 0.9 0)))
        , test "Scale alpha with negative value" <| \() -> (Expect.equal (hsla (degrees 120) 0.3 0.9 0.86) (scaleHsl ( 0, 0, -0.14 ) (hsl (degrees 120) 0.3 0.9)))
        , test "Scale red channel with positive value" <| \() -> (Expect.equal (rgb 186 20 30) (scaleRgb ( 0.3, 0, 0, 0 ) (rgb 157 20 30)))
        , test "Scale red channel with negative value" <| \() -> (Expect.equal (rgb 110 20 30) (scaleRgb ( -0.3, 0, 0, 0 ) (rgb 157 20 30)))
        , test "Scale green channel with positive value" <| \() -> (Expect.equal (rgb 157 55 30) (scaleRgb ( 0, 0.15, 0, 0 ) (rgb 157 20 30)))
        , test "Scale green channel with negative value" <| \() -> (Expect.equal (rgb 157 17 30) (scaleRgb ( 0, -0.15, 0, 0 ) (rgb 157 20 30)))
        , test "Scale blue channel with positive value" <| \() -> (Expect.equal (rgb 157 20 62) (scaleRgb ( 0, 0, 0.14, 0 ) (rgb 157 20 30)))
        , test "Scale blue channel with negative value" <| \() -> (Expect.equal (rgb 157 20 26) (scaleRgb ( 0, 0, -0.14, 0 ) (rgb 157 20 30)))
        , test "Scale alpha channel with positive value" <| \() -> (Expect.equal (rgba 157 20 30 0.6) (scaleRgb ( 0, 0, 0, 0.2 ) (rgba 157 20 30 0.5)))
        , test "Scale alpha channel with negative value" <| \() -> (Expect.equal (rgba 157 20 30 0.4) (scaleRgb ( 0, 0, 0, -0.2 ) (rgba 157 20 30 0.5)))
        , test "Mix 1" <| \() -> (Expect.equal (rgb 128 0 128) (mix (rgb 255 0 0) (rgb 0 0 255)))
        , test "Mix 2" <| \() -> (Expect.equal (rgb 128 128 128) (mix (rgb 255 255 0) (rgb 0 0 255)))
        , test "Mix 3" <| \() -> (Expect.equal (rgb 128 145 85) (mix (rgb 255 119 0) (rgb 0 170 170)))
        , test "Mix 4" <| \() -> (Expect.equal (rgb 64 0 191) (weightedMix (rgb 255 0 0) (rgb 0 0 255) 0.25))
        , test "Mix 5" <| \() -> (Expect.equal (rgba 64 0 191 0.75) (mix (rgba 255 0 0 0.5) (rgb 0 0 255)))
        , test "Mix 6" <| \() -> (Expect.equal (rgb 255 0 0) (weightedMix (rgb 255 0 0) (rgb 0 0 255) 1))
        , test "Mix 7" <| \() -> (Expect.equal (rgb 0 0 255) (weightedMix (rgb 255 0 0) (rgb 0 0 255) 0))
        , test "Mix 8" <| \() -> (Expect.equal (rgba 255 0 0 0.5) (mix (rgb 255 0 0) (rgba 0 0 255 0)))
        , test "Mix 9" <| \() -> (Expect.equal (rgba 0 0 255 0.5) (mix (rgba 255 0 0 0) (rgb 0 0 255)))
        , test "Mix 10" <| \() -> (Expect.equal (rgb 255 0 0) (weightedMix (rgb 255 0 0) (rgba 0 0 255 0) 1))
        , test "Mix 11" <| \() -> (Expect.equal (rgb 0 0 255) (weightedMix (rgba 255 0 0 0) (rgb 0 0 255) 0))
        , test "Mix 12" <| \() -> (Expect.equal (rgba 0 0 255 0) (weightedMix (rgb 255 0 0) (rgba 0 0 255 0) 0))
        , test "Mix 13" <| \() -> (Expect.equal (rgba 255 0 0 0) (weightedMix (rgba 255 0 0 0) (rgb 0 0 255) 1))
        ]


c1 : Color
c1 =
    (rgb 255 102 0)


c2 : Color
c2 =
    (rgb 0 255 0)


blending : Test
blending =
    describe "Blending"
        [ test "Multiply" <| \() -> (Expect.equal (multiply c1 c2) (rgb 0 102 0))
        , test "Screen" <| \() -> (Expect.equal (screen c1 c2) (rgb 255 255 0))
        , test "Overlay" <| \() -> (Expect.equal (overlay c1 c2) (rgb 255 204 0))
        , test "Softlight" <| \() -> (Expect.equal (softlight c1 c2) (rgb 255 161 0))
        , test "Hardlight" <| \() -> (Expect.equal (hardlight c1 c2) (c2))
        , test "Difference" <| \() -> (Expect.equal (difference c1 c2) (rgb 255 153 0))
        , test "Exclusion" <| \() -> (Expect.equal (exclusion c1 c2) (rgb 255 153 0))
        , test "Darken" <| \() -> (Expect.equal (Ble.darken c1 c2) (rgb 0 102 0))
        , test "Lighten" <| \() -> (Expect.equal (Ble.lighten c1 c2) (rgb 255 255 0))
        ]


interpolation : Test
interpolation =
    describe "Interpolate"
        [ test "Mix" <| \() -> (Expect.equal (interpolate RGB (rgba 0 0 0 0) (rgba 255 255 255 1) 0.5) (rgba 128 128 128 0.5))
        ]


p1 : Palette
p1 =
    [ rgb 200 0 200
    , rgb 0 100 100
    , rgb 100 0 0
    ]


p1Result : Palette
p1Result =
    [ rgb 200 0 200
    , rgb 100 50 150
    , rgb 0 100 100
    , rgb 50 50 50
    , rgb 100 0 0
    ]


p2 : Gradient
p2 =
    [ ( 0, rgb 200 0 200 )
    , ( 0.25, rgb 0 100 100 )
    , ( 1, rgb 150 175 160 )
    ]


p2Result : Palette
p2Result =
    [ rgb 200 0 200
    , rgb 0 100 100
    , rgb 50 125 120
    , rgb 100 150 140
    , rgb 150 175 160
    ]


gradient : Test
gradient =
    describe "Gradient"
        [ test "Gradient from list" <| \() -> (Expect.equal (Gra.gradient RGB p1 5) p1Result)
        , test "Gradient from stops" <| \() -> (Expect.equal (Gra.gradientFromStops RGB p2 5) p2Result)
        ]


all : Test
all =
    describe "All tests"
        [ accessibility
        , convert
        , manipulate
        , blending
        , gradient
        , interpolation
        ]
