module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Date as Date exposing (Date)
import Html as H exposing (Html)
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (..)
import Task
import Time exposing (..)


type Msg
    = NoOp
    | CheckTime Posix


type alias Model =
    { countdown : Posix
    , end : Posix
    }


background : List String
background =
    [ "http://getwallpapers.com/wallpaper/full/5/a/2/341101.jpg" ]


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


dayView : Int -> Int -> HS.Html msg
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
    span [ class "days" ] [ text (String.fromInt day), text label ]


clockView : Int -> Int -> Int -> HS.Html msg
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


view : Model -> H.Html Msg
view ({ countdown, end } as model) =
    let
        days =
            Date.diff Date.Days (Date.fromPosix utc countdown) (Date.fromPosix utc end)

        hours =
            if Time.toHour utc countdown > Time.toHour utc end then
                end
                    |> Time.toHour utc
                    |> (-) (Time.toHour utc countdown)
                    |> (-) 24

            else
                countdown
                    |> Time.toHour utc
                    |> (-) (Time.toHour utc end)

        min =
            if Time.toMinute utc countdown == 0 then
                countdown
                    |> Time.toMinute utc

            else
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
        , img [ src (List.head background |> Maybe.withDefault "") ] []
        ]
        |> HS.toUnstyled


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
