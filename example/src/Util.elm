module Util exposing (..)

import Color exposing (Color, rgb)
import Color.Manipulate exposing (..)


type alias Model =
    { darken :
        { args : List ( Arg, Arg -> Msg )
        , result : Color
        }
    }


initial v cl fn =
    ( v, cl, fn v cl )


model : Model
model =
    { darken =
        { args = []
        , result = darken 0.5 (rgb 1 1 1)
        }
    }


type Msg
    = Darken Arg Arg


type Arg
    = F Float
    | C Color


toMsgs2 : ( Arg, Arg ) -> (Arg -> Arg -> Msg) -> List ( Arg, Arg -> Msg )
toMsgs2 ( a, b ) fn =
    [ ( a, flip fn b ), ( b, fn a ) ]


la =
    toMsgs2 ( F 1, C (Color.rgb 1 1 1) ) Darken


update : Msg -> Model -> Model
update msg model =
    case msg of
        Darken v cl ->
            { model
                | darken =
                    { args = []
                    , result = darken v cl
                    }
            }
