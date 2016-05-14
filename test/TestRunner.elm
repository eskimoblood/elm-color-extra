module Main exposing (..)

import ElmTest exposing (runSuite)
import Tests


main : Program Never
main =
    runSuite Tests.all
