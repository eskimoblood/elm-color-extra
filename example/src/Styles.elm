module Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, span)
import Css.Namespace exposing (namespace)


type CssClasses
    = Swatch


css =
    (stylesheet << namespace "colors")
        [ body
            [ overflowX auto
            , minWidth (px 1280)
            ]
        , class Swatch
            [ position relative
            , children
                [ span
                    [ position absolute
                    , left zero
                    , color (rgb 255 255 255)
                    ]
                ]
            ]
        ]


primaryAccentColor =
    hex "ccffaa"
