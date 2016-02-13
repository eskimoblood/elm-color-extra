module Color.Gradient (gradient, gradientFromStops, Palette, GradientStop, Gradient) where

{-|
# Gradient
@docs GradientStop, Gradient, Palette, gradient, gradientFromStops
-}

import Color exposing (Color)
import Maybe exposing (..)
import Color.Interpolate as Interpolate exposing (interpolate, Space(RGB, HSL))


{-| Create a new gradient `Palette` from a given `Palette`, with a given size.
-}
{-| -}
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
gradient : Space -> Palette -> Int -> Palette
gradient space palette size =
    let
        l = List.length palette - 1

        gr = List.map2 (\i cl -> ( (toFloat i / toFloat l), cl )) [0..l] palette
    in
        gradientFromStops space gr size


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
gradientFromStops : Space -> Gradient -> Int -> Palette
gradientFromStops space stops size =
    let
        purifiedStops =
            stops
                |> List.filter (\( t, _ ) -> t >= 0 && t <= 1)
                |> List.sortBy (\( t, _ ) -> t)

        stop1 = List.head purifiedStops
    in
        case stop1 of
            Just s1 ->
                let
                    l = size - 1

                    stops = [0..l] |> List.map (\i -> (toFloat i) / l)

                    currentStops = Maybe.withDefault [] (List.tail purifiedStops)

                    ( s2, g ) = getNextGradientStop s1 currentStops
                in
                    List.foldl (c space) ( s1, s2, g, [] ) stops
                        |> (\( _, _, _, p ) -> p)
                        |> List.reverse

            Nothing ->
                []


c : Space -> Float -> ( GradientStop, GradientStop, Gradient, Palette ) -> ( GradientStop, GradientStop, Gradient, Palette )
c space t ( stop1, stop2, gradient, palette ) =
    let
        ( stop1', stop2', gradient', color ) = calculateGradient space stop1 stop2 gradient t
    in
        ( stop1', stop2', gradient', (color :: palette) )


calculateGradient : Space -> GradientStop -> GradientStop -> Gradient -> Float -> ( GradientStop, GradientStop, Gradient, Color )
calculateGradient space stop1 stop2 gradient t =
    if (fst stop2 < t) then
        let
            stop1' = stop2

            ( stop2', gradient' ) = getNextGradientStop stop2 gradient
        in
            ( stop1', stop2', gradient', (calculateColor space stop1' stop2' t) )
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
        nextStop = List.head gradient
    in
        case nextStop of
            Just s ->
                ( s, Maybe.withDefault [] (List.tail gradient) )

            Nothing ->
                ( currentStop, gradient )
