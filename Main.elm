module Main exposing (..)

import Html exposing (text)


-- Model


type Status
    = Relax
    | Focus


type Mode
    = Elapsed
    | Remaining


type alias Model =
    { counting : Bool
    , timerStatus : Status
    , timerMode : Mode
    , seconds : Int
    , pomsCompleted : Int
    }



-- Update
-- View
-- Init


main =
    text "Ahoy world!"
