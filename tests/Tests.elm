module Tests exposing (accessibility, all, blending, c1, c2, convert, gradient, interpolation, lab1, manipulate, p1, p1Result, p2, p2Result)

import Color exposing (Color, hsl, hsla, rgb, rgba)
import Color.Accessibility exposing (..)
import Color.Blending as Ble exposing (..)
import Color.Convert exposing (..)
import Color.Gradient as Gra exposing (..)
import Color.Interpolate as Int exposing (..)
import Color.Manipulate as Man exposing (..)
import Expect
import Test exposing (..)


accessibility : Test
accessibility =
    describe "Accessibility"
        [ test "Contrast ratio of black and white should be 21:1" <|
            \() ->
                contrastRatio Color.black Color.white
                    |> Expect.equal 21.0
        , test "Contrast ratio of equal colors should be 1:1" <|
            \() ->
                contrastRatio Color.blue Color.blue
                    |> Expect.within (Expect.Absolute 0.0000001) 1.0
        , test "Contrast ratio color order does not matter" <|
            \() ->
                Expect.within
                    (Expect.Absolute 0.0000001)
                    (contrastRatio Color.green Color.blue)
                    (contrastRatio Color.blue Color.green)
        , test "Luminance of black is the minimum possible" <|
            \() ->
                luminance Color.black
                    |> Expect.equal 0.0
        , test "Luminance of white is the maximum possible" <|
            \() ->
                luminance Color.white
                    |> Expect.equal 1.0
        , test "Maximum contrast" <|
            \() ->
                [ Color.white, Color.darkBlue, Color.green ]
                    |> maximumContrast Color.yellow
                    |> Expect.equal (Just Color.darkBlue)
        ]


convert : Test
convert =
    describe "Convert"
        [ test "Color to rgb String" <|
            \() ->
                colorToCssRgb (rgb 255 125 0)
                    |> Expect.equal "rgb(255, 125, 0)"
        , test "Color to rgba String" <|
            \() ->
                colorToCssRgba (rgba 255 125 0 0.3)
                    |> Expect.equal "rgba(255, 125, 0, 0.3)"
        , test "Color to hsl String" <|
            \() ->
                colorToCssHsl (hsl 0.4 0.2 0)
                    |> Expect.equal "hsl(23, 20%, 0%)"
        , test "Color to hsla String" <|
            \() ->
                colorToCssHsla (hsla 0.4 0.2 0 1)
                    |> Expect.equal "hsla(23, 20%, 0%, 1)"
        , test "Color to hex String" <|
            \() ->
                colorToHex (rgb 255 0 255)
                    |> Expect.equal "#ff00ff"
        , test "Color to hex String ignores alpha" <|
            \() ->
                colorToHex (rgba 255 0 255 0)
                    |> Expect.equal "#ff00ff"
        , test "Color to hex String with alpha keeps #RRGGBB format when alpha = 1" <|
            \() ->
                colorToHexWithAlpha (rgb 255 0 255)
                    |> Expect.equal "#ff00ff"
        , test "Color to hex String with alpha keeps #RRGGBB format when alpha = 1 (explicitly)" <|
            \() ->
                colorToHexWithAlpha (rgba 255 0 255 1)
                    |> Expect.equal "#ff00ff"
        , test "Color to hex String with alpha uses #RRGGBBAA format when alpha /= 1" <|
            \() ->
                colorToHexWithAlpha (rgba 255 0 255 0.5)
                    |> Expect.equal "#ff00ff80"
        , test "Hex string to hex color (#RRGGBB)" <|
            \() ->
                hexToColor "#ff00ff"
                    |> Expect.equal (Ok <| rgb 255 0 255)
        , test "Hex string to hex color (RRGGBB)" <|
            \() ->
                hexToColor "ff00ff"
                    |> Expect.equal (Ok <| rgb 255 0 255)
        , test "Hex string to hex color (#RGB)" <|
            \() ->
                hexToColor "#f0f"
                    |> Expect.equal (Ok <| rgb 255 0 255)
        , test "Hex string to hex color (RGB)" <|
            \() ->
                hexToColor "0a0"
                    |> Expect.equal (Ok <| rgb 0 170 0)
        , test "Hex string to hex color (#RRGGBBAA)" <|
            \() ->
                hexToColor "#ff00ff80"
                    |> Expect.equal (Ok <| rgba 255 0 255 0.5)
        , test "Hex string to hex color (RRGGBBAA)" <|
            \() ->
                hexToColor "ff00ff80"
                    |> Expect.equal (Ok <| rgba 255 0 255 0.5)
        , test "Hex string to hex color (#RGBA)" <|
            \() ->
                hexToColor "#f0f0"
                    |> Expect.equal (Ok <| rgba 255 0 255 0)
        , test "Hex string to hex color (RGBA)" <|
            \() ->
                hexToColor "f0f0"
                    |> Expect.equal (Ok <| rgba 255 0 255 0)
        , test "Hex string to hex color (fails)" <|
            \() ->
                hexToColor "12345"
                    |> Expect.equal (Err "Parsing hex regex failed")
        , test "Rgb to lab" <|
            \() ->
                colorToLab (rgb 255 255 0)
                    |> Expect.equal lab1
        , test "Lab to rgb" <|
            \() ->
                labToColor lab1
                    |> Expect.equal (rgb 255 255 0)
        ]


lab1 : { l : Float, a : Float, b : Float }
lab1 =
    { l = 97.13824698129729, a = -21.555908334832285, b = 94.48248544644461 }


manipulate : Test
manipulate =
    describe "Manipulate"
        [ test "Darken" <|
            \() ->
                Man.darken 0.5 (hsl 1 1 1)
                    |> Expect.equal (hsl 1 1 0.5)
        , test "Darken should be limit to 0" <|
            \() ->
                Man.darken 10 (hsl 1 1 1)
                    |> Expect.equal (hsl 1 1 0)
        , test "Lighten" <|
            \() ->
                Man.lighten 0.5 (hsl 1 1 0.2)
                    |> Expect.equal (hsl 1 1 0.7)
        , test "Lighten should be limit to 1" <|
            \() ->
                Man.lighten 10 (hsl 1 1 0)
                    |> Expect.equal (hsl 1 1 1)
        , test "Saturate" <|
            \() ->
                saturate 0.5 (hsl 1 0 1)
                    |> Expect.equal (hsl 1 0.5 1)
        , test "Saturate should be limit to 1" <|
            \() ->
                saturate 10 (hsl 1 1 1)
                    |> Expect.equal (hsl 1 1 1)
        , test "Desaturate" <|
            \() ->
                desaturate 0.5 (hsl 1 1 1)
                    |> Expect.equal (hsl 1 0.5 1)
        , test "Desaturate should be limit to 0" <|
            \() ->
                desaturate 10 (hsl 1 1 1)
                    |> Expect.equal (hsl 1 0 1)
        , test "Grayscale" <|
            \() ->
                Man.grayscale (hsl 1 1 1)
                    |> Expect.equal (hsl 1 0 1)
        , test "Fade in" <|
            \() ->
                fadeIn 0.2 (hsla 1 1 1 0.5)
                    |> Expect.equal (hsla 1 1 1 0.7)
        , test "Fade in should be limit to 1" <|
            \() ->
                fadeIn 10 (hsla 1 1 1 0.5)
                    |> Expect.equal (hsla 1 1 1 1)
        , test "Fade out" <|
            \() ->
                fadeOut 0.2 (hsla 1 1 1 0.5)
                    |> Expect.equal (hsla 1 1 1 0.3)
        , test "Fade out should be limit to 0" <|
            \() ->
                fadeOut 10 (hsla 1 1 1 0.5)
                    |> Expect.equal (hsla 1 1 1 0)
        , test "Rotate hue" <|
            \() ->
                rotateHue 90 (hsla 0 1 1 0)
                    |> Expect.equal (hsla (degrees 90) 1 1 0)
        , test "Rotate hue with negative value" <|
            \() ->
                rotateHue -90 (hsla 0 1 1 0)
                    |> Expect.equal (hsla (degrees 270) 1 1 0)
        , test "Rotate hue for more then 360°" <|
            \() ->
                rotateHue 270 (hsla (degrees 180) 1 1 0)
                    |> Expect.equal (hsla (degrees 90) 1 1 0)
        , test "Scale saturation with positive value" <|
            \() ->
                scaleHsl ( 0.3, 0, 0 ) (hsl (degrees 120) 0.3 0.9)
                    |> Expect.equal (hsl (degrees 120) 0.51 0.9)
        , test "Scale saturation with negative value" <|
            \() ->
                scaleHsl ( -0.3, 0, 0 ) (hsl (degrees 120) 0.3 0.9)
                    |> Expect.equal (hsl (degrees 120) 0.21 0.9)
        , test "Scale lightness with positive value" <|
            \() ->
                scaleHsl ( 0, 0.15, 0 ) (hsl (degrees 120) 0.3 0.9)
                    |> Expect.equal (hsl (degrees 120) 0.3 0.915)
        , test "Scale lightness with negative value" <|
            \() ->
                scaleHsl ( 0, -0.15, 0 ) (hsl (degrees 120) 0.3 0.9)
                    |> Expect.equal (hsl (degrees 120) 0.3 0.765)
        , test "Scale alpha with positive value" <|
            \() ->
                scaleHsl ( 0, 0, 0.14 ) (hsla (degrees 120) 0.3 0.9 0)
                    |> Expect.equal (hsla (degrees 120) 0.3 0.9 0.14)
        , test "Scale alpha with negative value" <|
            \() ->
                scaleHsl ( 0, 0, -0.14 ) (hsl (degrees 120) 0.3 0.9)
                    |> Expect.equal (hsla (degrees 120) 0.3 0.9 0.86)
        , test "Scale red channel with positive value" <|
            \() ->
                rgb 157 20 30
                    |> scaleRgb { red = 0.3, green = 0, blue = 0, alpha = 0 }
                    |> Expect.equal (rgb 186 20 30)
        , test "Scale red channel with negative value" <|
            \() ->
                rgb 157 20 30
                    |> scaleRgb { red = -0.3, green = 0, blue = 0, alpha = 0 }
                    |> Expect.equal (rgb 110 20 30)
        , test "Scale green channel with positive value" <|
            \() ->
                rgb 157 20 30
                    |> scaleRgb { red = 0, green = 0.15, blue = 0, alpha = 0 }
                    |> Expect.equal (rgb 157 55 30)
        , test "Scale green channel with negative value" <|
            \() ->
                rgb 157 20 30
                    |> scaleRgb { red = 0, green = -0.15, blue = 0, alpha = 0 }
                    |> Expect.equal (rgb 157 17 30)
        , test "Scale blue channel with positive value" <|
            \() ->
                rgb 157 20 30
                    |> scaleRgb { red = 0, green = 0, blue = 0.14, alpha = 0 }
                    |> Expect.equal (rgb 157 20 62)
        , test "Scale blue channel with negative value" <|
            \() ->
                rgb 157 20 30
                    |> scaleRgb { red = 0, green = 0, blue = -0.14, alpha = 0 }
                    |> Expect.equal (rgb 157 20 26)
        , test "Scale alpha channel with positive value" <|
            \() ->
                rgba 157 20 30 0.5
                    |> scaleRgb { red = 0, green = 0, blue = 0, alpha = 0.2 }
                    |> Expect.equal (rgba 157 20 30 0.6)
        , test "Scale alpha channel with negative value" <|
            \() ->
                rgba 157 20 30 0.5
                    |> scaleRgb { red = 0, green = 0, blue = 0, alpha = -0.2 }
                    |> Expect.equal (rgba 157 20 30 0.4)
        , test "Mix 1" <|
            \() ->
                mix (rgb 255 0 0) (rgb 0 0 255)
                    |> Expect.equal (rgb 128 0 128)
        , test "Mix 2" <|
            \() ->
                mix (rgb 255 255 0) (rgb 0 0 255)
                    |> Expect.equal (rgb 128 128 128)
        , test "Mix 3" <|
            \() ->
                mix (rgb 255 119 0) (rgb 0 170 170)
                    |> Expect.equal (rgb 128 145 85)
        , test "Mix 4" <|
            \() ->
                weightedMix (rgb 255 0 0) (rgb 0 0 255) 0.25
                    |> Expect.equal (rgb 64 0 191)
        , test "Mix 5" <|
            \() ->
                mix (rgba 255 0 0 0.5) (rgb 0 0 255)
                    |> Expect.equal (rgba 64 0 191 0.75)
        , test "Mix 6" <|
            \() ->
                weightedMix (rgb 255 0 0) (rgb 0 0 255) 1
                    |> Expect.equal (rgb 255 0 0)
        , test "Mix 7" <|
            \() ->
                weightedMix (rgb 255 0 0) (rgb 0 0 255) 0
                    |> Expect.equal (rgb 0 0 255)
        , test "Mix 8" <|
            \() ->
                mix (rgb 255 0 0) (rgba 0 0 255 0)
                    |> Expect.equal (rgba 255 0 0 0.5)
        , test "Mix 9" <|
            \() ->
                mix (rgba 255 0 0 0) (rgb 0 0 255)
                    |> Expect.equal (rgba 0 0 255 0.5)
        , test "Mix 10" <|
            \() ->
                weightedMix (rgb 255 0 0) (rgba 0 0 255 0) 1
                    |> Expect.equal (rgb 255 0 0)
        , test "Mix 11" <|
            \() ->
                weightedMix (rgba 255 0 0 0) (rgb 0 0 255) 0
                    |> Expect.equal (rgb 0 0 255)
        , test "Mix 12" <|
            \() ->
                weightedMix (rgb 255 0 0) (rgba 0 0 255 0) 0
                    |> Expect.equal (rgba 0 0 255 0)
        , test "Mix 13" <|
            \() ->
                weightedMix (rgba 255 0 0 0) (rgb 0 0 255) 1
                    |> Expect.equal (rgba 255 0 0 0)
        ]


c1 : Color
c1 =
    rgb 255 102 0


c2 : Color
c2 =
    rgb 0 255 0


blending : Test
blending =
    describe "Blending"
        [ test "Multiply" <|
            \() ->
                multiply c1 c2
                    |> Expect.equal (rgb 0 102 0)
        , test "Screen" <|
            \() ->
                screen c1 c2
                    |> Expect.equal (rgb 255 255 0)
        , test "Overlay" <|
            \() ->
                overlay c1 c2
                    |> Expect.equal (rgb 255 204 0)
        , test "Softlight" <|
            \() ->
                softlight c1 c2
                    |> Expect.equal (rgb 255 161 0)
        , test "Hardlight" <|
            \() ->
                hardlight c1 c2
                    |> Expect.equal c2
        , test "Difference" <|
            \() ->
                difference c1 c2
                    |> Expect.equal (rgb 255 153 0)
        , test "Exclusion" <|
            \() ->
                exclusion c1 c2
                    |> Expect.equal (rgb 255 153 0)
        , test "Darken" <|
            \() ->
                Ble.darken c1 c2
                    |> Expect.equal (rgb 0 102 0)
        , test "Lighten" <|
            \() ->
                Ble.lighten c1 c2
                    |> Expect.equal (rgb 255 255 0)
        ]


interpolation : Test
interpolation =
    describe "Interpolate"
        [ test "Mix" <|
            \() ->
                interpolate RGB (rgba 0 0 0 0) (rgba 255 255 255 1) 0.5
                    |> Expect.equal (rgba 128 128 128 0.5)
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
        [ test "Gradient from list" <|
            \() ->
                Gra.linearGradient RGB p1 5
                    |> Expect.equal p1Result
        , test "Gradient from stops" <|
            \() ->
                Gra.linearGradientFromStops RGB p2 5
                    |> Expect.equal p2Result
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
