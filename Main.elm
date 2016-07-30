module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (class, src, href)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
import String


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
    , chilloutMode : Bool
    }



-- Number of seconds in chillout mode


chilloutLimit : Int
chilloutLimit =
    20 * 60



-- Number of seconds for a normal relax mode


relaxLimit : Int
relaxLimit =
    5 * 60



-- Number of seconds for a normal focus mode


focusLimit : Int
focusLimit =
    25 * 60



-- Update


type Msg
    = Tick Time
    | Start
    | Pause
    | Clear
    | ElapsedMode
    | RemainingMode


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
            ( { model | timerStatus = Focus }
                |> stopCounting
                |> zeroClock
                |> resetPomsCompleted
            , Cmd.none
            )

        ElapsedMode ->
            ( { model | timerMode = Elapsed }, Cmd.none )

        RemainingMode ->
            ( { model | timerMode = Remaining }, Cmd.none )

        Tick _ ->
            case
                ( model.counting
                , model.timerStatus
                , (model.seconds == focusLimit)
                , (model.seconds == relaxLimit)
                , (model.seconds == chilloutLimit)
                , model.chilloutMode
                )
            of
                -- Not counting, so do nothing
                ( False, _, _, _, _, _ ) ->
                    ( model, Cmd.none )

                -- Counting and the clock has struck 25 minutes in Focus
                ( True, Focus, True, _, _, _ ) ->
                    ( model
                        |> flipStatus
                        |> zeroClock
                    , Cmd.none
                    )

                -- Counting and clock has struck 5 minutes in Relax
                ( True, Relax, _, True, _, False ) ->
                    ( model
                        |> flipStatus
                        |> zeroClock
                        |> markPomsCompleted
                    , Cmd.none
                    )

                -- Exit chilloutMode
                ( True, Relax, _, _, True, True ) ->
                    ( model
                        |> flipStatus
                        |> zeroClock
                        |> markPomsCompleted
                        |> disengageChilloutMode
                    , Cmd.none
                    )

                -- Ordinary counting
                ( True, _, _, _, _, _ ) ->
                    ( model
                        |> tickSecond model.seconds
                    , Cmd.none
                    )


disengageChilloutMode : Model -> Model
disengageChilloutMode model =
    { model | chilloutMode = False }


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
    case
        ( model.timerStatus
        , (rem model.pomsCompleted 4 == 0)
            && (model.pomsCompleted > 0)
        )
    of
        ( Focus, False ) ->
            { model | timerStatus = Relax }

        ( Focus, True ) ->
            { model
                | timerStatus = Relax
                , chilloutMode = True
            }

        ( Relax, _ ) ->
            { model
                | timerStatus = Focus
                , chilloutMode = False
            }


zeroClock : Model -> Model
zeroClock model =
    { model | seconds = 0 }


markPomsCompleted : Model -> Model
markPomsCompleted model =
    { model | pomsCompleted = model.pomsCompleted + 1 }



-- View


view : Model -> Html Msg
view model =
    div [ class "appWindow" ]
        [ makeHeader
        , p [ class "counter" ]
            [ text <| "Routines Completed: " ++ toString model.pomsCompleted ]
        , makeMainPage model
          --- Purely for sanity checking will be removed later
        , makeFooter
        ]


makeMainPage : Model -> Html Msg
makeMainPage model =
    div []
        [ makeClock model
        , makeButtonCluster
        ]


makeButtonCluster : Html Msg
makeButtonCluster =
    div [ class "btncluster" ]
        [ button [ onClick Start ] [ text "Start" ]
        , button [ onClick Pause ] [ text "Pause" ]
        , button [ onClick Clear ] [ text "Clear" ]
        ]


makeClock : Model -> Html Msg
makeClock model =
    div []
        [ div [ bezelChecker model.timerStatus ]
            [ div [ statusChecker model.timerStatus ]
                [ text <| toString model.timerStatus
                ]
            , div [ gaugeChecker model.timerStatus ]
                [ text <| timeMaker model
                ]
            , bezelButtonMaker "Elapsed" ElapsedMode model
            , bezelButtonMaker "Remaining" RemainingMode model
            ]
        ]


timeMaker : Model -> String
timeMaker model =
    case
        ( model.timerMode
        , model.timerStatus
        , model.chilloutMode
        , model.seconds
        )
    of
        ( Elapsed, _, _, s ) ->
            getClockString s

        ( Remaining, Relax, False, s ) ->
            getClockString <| (relaxLimit - s)

        ( Remaining, Relax, True, s ) ->
            getClockString <| (chilloutLimit - s)

        ( Remaining, Focus, _, s ) ->
            getClockString <| (focusLimit - s)


getClockString : Int -> String
getClockString sec =
    let
        formatter x =
            if (String.length <| toString x) == 1 then
                "0" ++ toString x
            else
                toString x

        madeMinutes =
            sec // 60

        madeSeconds =
            rem sec 60
    in
        formatter madeMinutes ++ " : " ++ formatter madeSeconds


bezelChecker : Status -> Html.Attribute Msg
bezelChecker status =
    case status of
        Relax ->
            class "bezelrelax"

        Focus ->
            class "bezelfocus"


statusChecker : Status -> Html.Attribute Msg
statusChecker status =
    case status of
        Relax ->
            class "statusrelax"

        Focus ->
            class "statusfocus"


gaugeChecker : Status -> Html.Attribute Msg
gaugeChecker status =
    case status of
        Relax ->
            class "gaugerelax"

        Focus ->
            class "gaugefocus"


bezelButtonMaker : String -> Msg -> Model -> Html Msg
bezelButtonMaker btnName msg model =
    button
        [ onClick msg, getBezelBtnClass btnName model ]
        [ text btnName ]


getBezelBtnClass : String -> Model -> Html.Attribute Msg
getBezelBtnClass btnName model =
    if btnName == (toString model.timerMode) then
        class "activebezelbtn"
    else
        class "inactivebezelbtn"


makeHeader : Html Msg
makeHeader =
    header [ class "full-width-bar" ]
        [ div []
            [ h2 [] [ text "Focus Optimizer" ]
            ]
        ]


makeFooter : Html Msg
makeFooter =
    footer [ class "full-width-bar-bottom" ]
        [ div [ class "right" ]
            [ div [] [ linkMaker ]
            ]
        , div []
            [ img [ src "Signature.png", class "center-cropped" ] [] ]
        ]


linkMaker : Html Msg
linkMaker =
    ul [] <| List.map linkRenderer getLinks


getLinks : List ( String, String )
getLinks =
    [ ( "Medium", "https://medium.com/@NewMountain" )
    , ( "Twitter", "https://twitter.com/@nyberg_c" )
    , ( "GitHub", "https://github.com/NewMountain" )
    ]


linkRenderer : ( String, String ) -> Html Msg
linkRenderer ( name, url' ) =
    li []
        [ a [ href url' ] [ text name ]
        ]



-- Init


init : ( Model, Cmd Msg )
init =
    ( { counting = False
      , timerStatus = Focus
      , timerMode = Elapsed
      , seconds = 0
      , pomsCompleted = 0
      , chilloutMode = False
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
