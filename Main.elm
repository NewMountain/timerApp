module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Time exposing (Time, second)


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


type Msg
    = Tick Time
    | Start
    | Pause
    | Clear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- I don't care about the time, I just want the tick
        Start ->
            ( model
                |> startCounting
            , Cmd.none
            )

        Pause ->
            ( model
                |> stopCounting
            , Cmd.none
            )

        Clear ->
            ( model
                |> stopCounting
                |> zeroClock
                |> resetPomsCompleted
            , Cmd.none
            )

        Tick _ ->
            case
                ( model.counting
                , model.timerStatus
                , model.seconds
                )
            of
                -- Not counting, so do nothing
                ( False, _, _ ) ->
                    ( model, Cmd.none )

                -- Counting and the clock has struck 25 minutes in Focus
                ( True, Focus, 1500 ) ->
                    ( model
                        |> flipStatus
                        |> zeroClock
                    , Cmd.none
                    )

                -- Counting and clock has struck 5 minutes in Relax
                ( True, Relax, 300 ) ->
                    ( model
                        |> flipStatus
                        |> zeroClock
                        |> markPomsCompleted
                    , Cmd.none
                    )

                -- Ordinary counting
                ( True, _, s ) ->
                    ( model
                        |> tickSecond s
                    , Cmd.none
                    )


resetPomsCompleted : Model -> Model
resetPomsCompleted model =
    { model | pomsCompleted = 0 }


stopCounting : Model -> Model
stopCounting model =
    { model | counting = False }


startCounting : Model -> Model
startCounting model =
    { model | counting = True }


tickSecond : Int -> Model -> Model
tickSecond s model =
    { model | seconds = s + 1 }


flipStatus : Model -> Model
flipStatus model =
    case (model.timerStatus) of
        Focus ->
            { model | timerStatus = Relax }

        Relax ->
            { model | timerStatus = Focus }


zeroClock : Model -> Model
zeroClock model =
    { model | seconds = 0 }


markPomsCompleted : Model -> Model
markPomsCompleted model =
    { model | pomsCompleted = model.pomsCompleted + 1 }



-- View


view : Model -> Html Msg
view model =
    div []
        [ text "Ahoy world!"
        , p [] [ text <| toString model.seconds ]
        , br [] []
        , button [ onClick Start ] [ text "Start" ]
        , button [ onClick Pause ] [ text "Pause" ]
        , button [ onClick Clear ] [ text "Clear" ]
        , br [] []
        , p [] [ text <| toString model ]
        ]



-- Init


init : ( Model, Cmd Msg )
init =
    ( { counting = False
      , timerStatus = Relax
      , timerMode = Elapsed
      , seconds = 0
      , pomsCompleted = 0
      }
    , Cmd.none
    )



-- Subscription
-- Trust me on this one (we can make MUVIS instead!!!)


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- Main


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
