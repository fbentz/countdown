module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Date as Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Time exposing (..)


type Msg
    = NoOp
    | CheckTime Posix


type alias Model =
    { countdown : Posix
    , end : Posix
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { countdown = millisToPosix 0, end = Time.millisToPosix 1556391600000 }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CheckTime posix ->
            ( { model | countdown = posix }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrame CheckTime


dayView : Int -> Int -> Html msg
dayView days hours =
    let
        label =
            case days of
                0 ->
                    " jour"

                1 ->
                    " jour"

                _ ->
                    " jours"

        day =
            if hours > 21 then
                days - 1

            else
                days
    in
    span [ class "days" ] [ text (String.fromInt days), text label ]


clockView : Int -> Int -> Int -> Html msg
clockView hours minute second =
    let
        format int =
            int
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    span [ class "clock" ]
        [ hours
            |> format
            |> text
        , text " : "
        , minute
            |> format
            |> text
        , text " : "
        , second
            |> format
            |> text
        ]


view : Model -> Html Msg
view ({ countdown, end } as model) =
    let
        days =
            Date.diff Date.Days (Date.fromPosix utc countdown) (Date.fromPosix utc end)

        hours =
            countdown
                |> Time.toHour utc
                |> (-) (Time.toHour utc end)

        min =
            countdown
                |> Time.toMinute utc
                |> (-) 60

        sec =
            countdown
                |> Time.toSecond utc
                |> (-) 59
    in
    div [ class "countdown" ]
        [ dayView days hours
        , clockView hours min sec
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
