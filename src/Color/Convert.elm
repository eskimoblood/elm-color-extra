module Color.Convert (..) where

import ParseInt exposing (parseIntHex)
import Color exposing (..)
import Regex
import Array
import String
import Result
import List


colorToCssRgb : Color -> String
colorToCssRgb cl =
    let
        { red, green, blue, alpha } = toRgb cl
    in
        cssColorString "rgb" [ (toFloat red), (toFloat green), (toFloat blue) ]


colorToCssRgba : Color -> String
colorToCssRgba cl =
    let
        { red, green, blue, alpha } = toRgb cl
    in
        cssColorString "rgba" [ (toFloat red), (toFloat green), (toFloat blue), alpha ]


colorToCssHsl : Color -> String
colorToCssHsl cl =
    let
        { hue, saturation, lightness, alpha } = toHsl cl
    in
        cssColorString "hsl" [ hue, saturation, lightness ]


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
