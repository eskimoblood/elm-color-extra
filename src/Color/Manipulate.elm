module Color.Manipulate (darken, lighten, saturate, desaturate, rotateHue, fadeIn, fadeOut, grayscale) where

{-| A library for creating and manipulating colors.


# Color adjustment
@docs darken, lighten, saturate, desaturate, rotateHue, fadeIn, fadeOut, grayscale

-}

import Color exposing (Color, toHsl, hsla, toRgb, rgba)


limit : Float -> Float
limit =
    clamp 0 1


{-| Decrease the lightning of a color
-}
darken : Float -> Color -> Color
darken offset cl =
    let
        { hue, saturation, lightness, alpha } = toHsl cl
    in
        hsla hue saturation (limit (lightness + offset)) alpha


{-| Increase the lightning of a color
-}
lighten : Float -> Color -> Color
lighten offset cl =
    darken -offset cl


{-| Increase the saturation of a color
-}
saturate : Float -> Color -> Color
saturate offset cl =
    let
        { hue, saturation, lightness, alpha } = toHsl cl
    in
        hsla hue (limit (saturation + offset)) lightness alpha


{-| Decrease the saturation of a color
-}
desaturate : Float -> Color -> Color
desaturate offset cl =
    saturate -offset cl


{-| Convert the color to a greyscale version, aka set saturation to 0
-}
grayscale : Color -> Color
grayscale cl =
    saturate -1 cl


{-| Increase the opacity of a color
-}
fadeIn : Float -> Color -> Color
fadeIn offset cl =
    let
        { hue, saturation, lightness, alpha } = toHsl cl
    in
        hsla hue saturation lightness (limit (alpha + offset))


{-| Decrease the opacity of a color
-}
fadeOut : Float -> Color -> Color
fadeOut offset cl =
    fadeIn -offset cl


{-| Change the hue of a color. The angle value must be in degrees
-}
rotateHue : Float -> Color -> Color
rotateHue angle cl =
    let
        { hue, saturation, lightness, alpha } = toHsl cl
    in
        hsla (hue + (degrees angle)) saturation lightness alpha
