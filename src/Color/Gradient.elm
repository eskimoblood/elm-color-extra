module Color.Gradient exposing (linearGradient, linearGradientFromStops, cosineGradient, Palette, GradientStop, Gradient, CosineGradientSetting)

{-|
# Gradient
@docs GradientStop, Gradient, Palette, linearGradient, linearGradientFromStops, CosineGradientSetting, cosineGradient
-}

import Color exposing (Color)
import Maybe exposing (..)
import Tuple exposing (first)
import Color.Interpolate as Interpolate exposing (interpolate, Space(RGB, HSL))


{-| Create a new gradient `Palette` from a given `Palette`, with a given size.
-}
type alias Palette =
    List Color


{-| A color and a stop value that indicates where the color appears in a gradient.
 The stop value must be between `0` and `1`.
-}
type alias GradientStop =
    ( Float, Color )


{-| -}
type alias Gradient =
    List GradientStop


{-| Create a new `Palette`  with gradient colors from a given `Palette`,
 with a given size.

    p1 : Palette
    p1 =
      [ rgb 200 0 200
      , rgb 0 100 100
      , rgb 100 0 0
      ]
    gradient RGB p1 5 -- [RGBA 200 0 200 1,RGBA 100 50 150 1,RGBA 0 100 100 1,RGBA 50 50 50 1,RGBA 100 0 0 1]
-}
linearGradient : Space -> Palette -> Int -> Palette
linearGradient space palette size =
    let
        l =
            List.length palette - 1

        gr =
            List.map2 (\i cl -> ( (toFloat i / toFloat l), cl )) (List.range 0 l) palette
    in
        linearGradientFromStops space gr size


{-| Create a new `Palette`  with gradient colors from a given `Gradient`,
 with a given size.

    g : Gradient
    g =
      [ (0, rgb 200 0 200)
      , (0.25, rgb 0 100 100)
      , (1, rgb 150 175 160)
      ]
    gradientFromStops RGB g 5 -- [RGBA 200 0 200 1,RGBA 0 100 100 1,RGBA 50 125 120 1,RGBA 100 150 140 1,RGBA 150 175 160 1]
-}
linearGradientFromStops : Space -> Gradient -> Int -> Palette
linearGradientFromStops space stops size =
    let
        purifiedStops =
            stops
                |> List.filter (\( t, _ ) -> t >= 0 && t <= 1)
                |> List.sortBy (\( t, _ ) -> t)

        stop1 =
            List.head purifiedStops
    in
        case stop1 of
            Just s1 ->
                let
                    l =
                        size - 1

                    stops =
                        (List.range 0 l) |> List.map (\i -> (toFloat i) / (toFloat l))

                    currentStops =
                        Maybe.withDefault [] (List.tail purifiedStops)

                    ( s2, g ) =
                        getNextGradientStop s1 currentStops
                in
                    List.foldl (c space) ( s1, s2, g, [] ) stops
                        |> (\( _, _, _, p ) -> p)
                        |> List.reverse

            Nothing ->
                []


c : Space -> Float -> ( GradientStop, GradientStop, Gradient, Palette ) -> ( GradientStop, GradientStop, Gradient, Palette )
c space t ( stop1, stop2, gradient, palette ) =
    let
        ( stop1_, stop2_, gradient_, color ) =
            calculateGradient space stop1 stop2 gradient t
    in
        ( stop1_, stop2_, gradient_, (color :: palette) )


calculateGradient : Space -> GradientStop -> GradientStop -> Gradient -> Float -> ( GradientStop, GradientStop, Gradient, Color )
calculateGradient space stop1 stop2 gradient t =
    if (first stop2 < t) then
        let
            stop1_ =
                stop2

            ( stop2_, gradient_ ) =
                getNextGradientStop stop2 gradient
        in
            ( stop1_, stop2_, gradient_, (calculateColor space stop1_ stop2_ t) )
    else
        ( stop1, stop2, gradient, (calculateColor space stop1 stop2 t) )


calculateColor : Space -> GradientStop -> GradientStop -> Float -> Color
calculateColor space ( t1, cl1 ) ( t2, cl2 ) t =
    if t == 0 then
        cl1
    else if t == 1 then
        cl2
    else
        interpolate space cl1 cl2 ((t - t1) / (t2 - t1))


getNextGradientStop : GradientStop -> Gradient -> ( GradientStop, Gradient )
getNextGradientStop currentStop gradient =
    let
        nextStop =
            List.head gradient
    in
        case nextStop of
            Just s ->
                ( s, Maybe.withDefault [] (List.tail gradient) )

            Nothing ->
                ( currentStop, gradient )


{-|
   parameters for calculate RGB values for cosine gradients
-}
type alias CosineGradientSetting =
    ( Float, Float, Float )


calcCosine : Float -> Float -> Float -> Float -> Float -> Int
calcCosine a b c d t =
    (a + b * cos (pi * 2 * (c * t + d)))
        |> (clamp 0 1)
        |> (*) 255
        |> round


calcCosineColor : CosineGradientSetting -> CosineGradientSetting -> CosineGradientSetting -> CosineGradientSetting -> Float -> Color
calcCosineColor ( oX, oY, oZ ) ( aX, aY, aZ ) ( fX, fY, fZ ) ( pX, pY, pZ ) t =
    Color.rgb
        (calcCosine oX aX fX pX t)
        (calcCosine oY aY fY pY t)
        (calcCosine oZ aZ fZ pZ t)


{-|
   Create a gradient based on the on an [idea by Iñigo Quílez](http://www.iquilezles.org/www/articles/palettes/palettes.htm)
   For an interactive example have a look at Karsten Schmidt's example from his [thi.ng library](http://dev.thi.ng/gradients/)
-}
cosineGradient : CosineGradientSetting -> CosineGradientSetting -> CosineGradientSetting -> CosineGradientSetting -> Int -> Palette
cosineGradient offset amp fmod phase l =
    List.range 0 l
        |> List.map (toFloat >> (*) (1.0 / toFloat l))
        |> List.map (calcCosineColor offset amp fmod phase)
