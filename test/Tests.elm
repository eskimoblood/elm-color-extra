module Tests exposing (..)

import ElmTest exposing (..)
import Color.Convert exposing (..)
import Color.Gradient as Gra exposing (..)
import Color.Manipulate as Man exposing (..)
import Color.Blending as Ble exposing (..)
import Color.Interpolate as Int exposing (..)
import Color exposing (Color, rgb, rgba, hsl, hsla)


convert : Test
convert =
    suite "Convert"
        [ test "Color to rgb String" (assertEqual (colorToCssRgb (rgb 255 125 0)) "rgb(255, 125, 0)")
        , test "Color to rgba String" (assertEqual (colorToCssRgba (rgba 255 125 0 0.3)) "rgba(255, 125, 0, 0.3)")
        , test "Color to hsl String" (assertEqual (colorToCssHsl (hsl 0.4 0.2 0)) "hsl(23, 20%, 0%)")
        , test "Color to hsla String" (assertEqual (colorToCssHsla (hsla 0.4 0.2 0 1)) "hsla(23, 20%, 0%, 1)")
        , test "Color to hex String" (assertEqual (colorToHex (rgb 255 0 255)) "#ff00ff")
        , test "Hex string to hex color" (assertEqual (hexToColor "#ff00ff") (Just (rgb 255 0 255)))
        , test "Hex string to hex color" (assertEqual (hexToColor "ff00ff") (Just (rgb 255 0 255)))
        , test "Hex string to hex color" (assertEqual (hexToColor "1234") Nothing)
        , test "Rgb to lab" (assertEqual lab1 (colorToLab (rgb 255 255 0)))
        , test "Lab to rgb" (assertEqual (rgb 255 255 0) (labToColor lab1))
        ]


lab1 : { l : Float, a : Float, b : Float }
lab1 =
    { l = 97.13824698129729, a = -21.555908334832285, b = 94.48248544644461 }


manipulate : Test
manipulate =
    suite "Manipulate"
        [ test "Darken" (assertEqual (Man.darken 0.5 (hsl 1 1 1)) (hsl 1 1 0.5))
        , test "Darken should be limit to 0" (assertEqual (Man.darken 10 (hsl 1 1 1)) (hsl 1 1 0))
        , test "Lighten" (assertEqual (Man.lighten 0.5 (hsl 1 1 0.2)) (hsl 1 1 0.7))
        , test "Lighten should be limit to 1" (assertEqual (Man.lighten 10 (hsl 1 1 0)) (hsl 1 1 1))
        , test "Saturate" (assertEqual (saturate 0.5 (hsl 1 0 1)) (hsl 1 0.5 1))
        , test "Saturate should be limit to 1" (assertEqual (saturate 10 (hsl 1 1 1)) (hsl 1 1 1))
        , test "Desaturate" (assertEqual (desaturate 0.5 (hsl 1 1 1)) (hsl 1 0.5 1))
        , test "Desaturate should be limit to 0" (assertEqual (desaturate 10 (hsl 1 1 1)) (hsl 1 0 1))
        , test "Grayscale" (assertEqual (Man.grayscale (hsl 1 1 1)) (hsl 1 0 1))
        , test "Fade in" (assertEqual (fadeIn 0.2 (hsla 1 1 1 0.5)) (hsla 1 1 1 0.7))
        , test "Fade in should be limit to 1" (assertEqual (fadeIn 10 (hsla 1 1 1 0.5)) (hsla 1 1 1 1))
        , test "Fade out" (assertEqual (fadeOut 0.2 (hsla 1 1 1 0.5)) (hsla 1 1 1 0.3))
        , test "Fade out should be limit to 0" (assertEqual (fadeOut 10 (hsla 1 1 1 0.5)) (hsla 1 1 1 0))
        , test "Rotate hue" (assertEqual (rotateHue 90 (hsla 0 1 1 0)) (hsla (degrees 90) 1 1 0))
        , test "Rotate hue with negative value" (assertEqual (rotateHue -90 (hsla 0 1 1 0)) (hsla (degrees 270) 1 1 0))
        , test "Rotate hue for more then 360°" (assertEqual (rotateHue 270 (hsla (degrees 180) 1 1 0)) (hsla (degrees 90) 1 1 0))
        , test "Scale saturation with positive value" (assertEqual (hsl (degrees 120) 0.51 0.9) (scaleHsl ( 0.3, 0, 0 ) (hsl (degrees 120) 0.3 0.9)))
        , test "Scale saturation with negative value" (assertEqual (hsl (degrees 120) 0.21 0.9) (scaleHsl ( -0.3, 0, 0 ) (hsl (degrees 120) 0.3 0.9)))
        , test "Scale lightness with positive value" (assertEqual (hsl (degrees 120) 0.3 0.915) (scaleHsl ( 0, 0.15, 0 ) (hsl (degrees 120) 0.3 0.9)))
        , test "Scale lightness with negative value" (assertEqual (hsl (degrees 120) 0.3 0.765) (scaleHsl ( 0, -0.15, 0 ) (hsl (degrees 120) 0.3 0.9)))
        , test "Scale alpha with positive value" (assertEqual (hsla (degrees 120) 0.3 0.9 0.14) (scaleHsl ( 0, 0, 0.14 ) (hsla (degrees 120) 0.3 0.9 0)))
        , test "Scale alpha with negative value" (assertEqual (hsla (degrees 120) 0.3 0.9 0.86) (scaleHsl ( 0, 0, -0.14 ) (hsl (degrees 120) 0.3 0.9)))
        , test "Scale red channel with positive value" (assertEqual (rgb 186 20 30) (scaleRgb ( 0.3, 0, 0, 0 ) (rgb 157 20 30)))
        , test "Scale red channel with negative value" (assertEqual (rgb 110 20 30) (scaleRgb ( -0.3, 0, 0, 0 ) (rgb 157 20 30)))
        , test "Scale green channel with positive value" (assertEqual (rgb 157 55 30) (scaleRgb ( 0, 0.15, 0, 0 ) (rgb 157 20 30)))
        , test "Scale green channel with negative value" (assertEqual (rgb 157 17 30) (scaleRgb ( 0, -0.15, 0, 0 ) (rgb 157 20 30)))
        , test "Scale blue channel with positive value" (assertEqual (rgb 157 20 62) (scaleRgb ( 0, 0, 0.14, 0 ) (rgb 157 20 30)))
        , test "Scale blue channel with negative value" (assertEqual (rgb 157 20 26) (scaleRgb ( 0, 0, -0.14, 0 ) (rgb 157 20 30)))
        , test "Scale alpha channel with positive value" (assertEqual (rgba 157 20 30 0.6) (scaleRgb ( 0, 0, 0, 0.2 ) (rgba 157 20 30 0.5)))
        , test "Scale alpha channel with negative value" (assertEqual (rgba 157 20 30 0.4) (scaleRgb ( 0, 0, 0, -0.2 ) (rgba 157 20 30 0.5)))
        , test "Mix1" (assertEqual (rgb 128 0 128) (mix (rgb 255 0 0) (rgb 0 0 255)))
        , test "Mix2" (assertEqual (rgb 128 128 128) (mix (rgb 255 255 0) (rgb 0 0 255)))
        , test "Mix3" (assertEqual (rgb 128 145 85) (mix (rgb 255 119 0) (rgb 0 170 170)))
        , test "Mix4" (assertEqual (rgb 64 0 191) (weightedMix (rgb 255 0 0) (rgb 0 0 255) 0.25))
        , test "Mix5" (assertEqual (rgba 64 0 191 0.75) (mix (rgba 255 0 0 0.5) (rgb 0 0 255)))
        , test "Mix6" (assertEqual (rgb 255 0 0) (weightedMix (rgb 255 0 0) (rgb 0 0 255) 1.0))
        , test "Mix7" (assertEqual (rgb 0 0 255) (weightedMix (rgb 255 0 0) (rgb 0 0 255) 0.0))
        , test "Mix8" (assertEqual (rgba 255 0 0 0.5) (mix (rgb 255 0 0) (rgba 0 0 255 0.0)))
        , test "Mix9" (assertEqual (rgba 0 0 255 0.5) (mix (rgba 255 0 0 0.0) (rgb 0 0 255)))
        , test "Mix10" (assertEqual (rgb 255 0 0) (weightedMix (rgb 255 0 0) (rgba 0 0 255 0.0) 1.0))
        , test "Mix11" (assertEqual (rgb 0 0 255) (weightedMix (rgba 255 0 0 0.0) (rgb 0 0 255) 0.0))
        , test "Mix12" (assertEqual (rgba 0 0 255 0.0) (weightedMix (rgb 255 0 0) (rgba 0 0 255 0.0) 0.0))
        , test "Mix13" (assertEqual (rgba 255 0 0 0.0) (weightedMix (rgba 255 0 0 0.0) (rgb 0 0 255) 1.0))
        ]


c1 : Color
c1 =
    (rgb 255 102 0)


c2 : Color
c2 =
    (rgb 0 255 0)


blending : Test
blending =
    suite "Blending"
        [ test "Multiply" (assertEqual (multiply c1 c2) (rgb 0 102 0))
        , test "Screen" (assertEqual (screen c1 c2) (rgb 255 255 0))
        , test "Overlay" (assertEqual (overlay c1 c2) (rgb 255 204 0))
        , test "Softlight" (assertEqual (softlight c1 c2) (rgb 255 161 0))
        , test "Hardlight" (assertEqual (hardlight c1 c2) (c2))
        , test "Difference" (assertEqual (difference c1 c2) (rgb 255 153 0))
        , test "Exclusion" (assertEqual (exclusion c1 c2) (rgb 255 153 0))
        , test "Darken" (assertEqual (Ble.darken c1 c2) (rgb 0 102 0))
        , test "Lighten" (assertEqual (Ble.lighten c1 c2) (rgb 255 255 0))
        ]


interpolation : Test
interpolation =
    suite "Interpolate"
        [ test "Mix" (assertEqual (interpolate RGB (rgba 0 0 0 0) (rgba 255 255 255 1) 0.5) (rgba 128 128 128 0.5))
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
    suite "Gradient"
        [ test "Gradient from list" (assertEqual (Gra.gradient RGB p1 5) p1Result)
        , test "Gradient from stops" (assertEqual (Gra.gradientFromStops RGB p2 5) p2Result)
        ]


all : Test
all =
    suite "All tests"
        [ convert
        , manipulate
        , blending
        , gradient
        , interpolation
        ]
