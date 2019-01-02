module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Css exposing (..)
import Css.Animations exposing (..)
import Css.Global exposing (body, everything, global, html)
import Date as Date exposing (Date)
import Html as H exposing (Html)
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (..)
import Random as Random
import Task
import Time exposing (..)


type Msg
    = NoOp
    | CheckTime Posix
    | UpdateBackground Posix
    | SetBackground Int


type alias Model =
    { countdown : Posix
    , end : Posix
    , background : String
    }


defaultTextRulesCss : Style
defaultTextRulesCss =
    Css.batch
        [ Css.property "text-shadow" "1px 2px 3px rgba(160, 33, 33, 0.9), 1px 2px 28px rgba(255, 255, 255, 0.9)"
        , color (rgba 255 255 255 1)
        , textAlign center
        ]


pictures : List String
pictures =
    [ "http://getwallpapers.com/wallpaper/full/5/a/2/341101.jpg"
    , "http://getwallpapers.com/wallpaper/full/b/6/1/341140.jpg"
    , "http://getwallpapers.com/wallpaper/full/6/6/5/341223.jpg"
    , "http://getwallpapers.com/wallpaper/full/5/9/6/341307.jpg"
    , "http://getwallpapers.com/wallpaper/full/f/e/b/341637.jpg"
    , "http://getwallpapers.com/wallpaper/full/1/3/0/340556.jpg"
    ]


oneOfPictures : Int -> Random.Generator Int
oneOfPictures int =
    Random.int 0 (int - 1)


init : () -> ( Model, Cmd Msg )
init () =
    ( { countdown = millisToPosix 0
      , end = Time.millisToPosix 1556391600000
      , background = List.head pictures |> Maybe.withDefault ""
      }
    , Random.generate SetBackground (oneOfPictures (List.length pictures))
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CheckTime posix ->
            ( { model | countdown = posix }, Cmd.none )

        UpdateBackground posix ->
            ( model, Random.generate SetBackground (oneOfPictures (List.length pictures)) )

        SetBackground int ->
            let
                newBackground =
                    List.indexedMap Tuple.pair pictures
                        |> List.filter (\( index, picture ) -> index == int)
                        |> List.head
                        |> Maybe.map (\( index, picture ) -> picture)
                        |> Maybe.withDefault ""
            in
            ( { model | background = newBackground }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrame CheckTime
        , Time.every (30 * 1000) UpdateBackground
        ]


dayView : Int -> Int -> Int -> HS.Html msg
dayView days hours boundedhours =
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
            if hours > boundedhours then
                days - 1

            else
                days
    in
    span [ css [ defaultTextRulesCss, fontSize (Css.rem 5) ] ] [ text (String.fromInt day), text label ]


clockView : Int -> Int -> Int -> HS.Html msg
clockView hours minute second =
    let
        format int =
            int
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    span [ css [ defaultTextRulesCss, padding2 (px 10) (px 30) ] ]
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


countdownView : Posix -> Posix -> HS.Html msg
countdownView countdown end =
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
    div
        [ css
            [ fontSize (Css.rem 3)
            , Css.property "display" "grid"
            , justifyContent center
            ]
        ]
        [ dayView days hours (Time.toHour utc end)
        , clockView hours min sec
        ]


view : Model -> H.Html Msg
view ({ countdown, end, background } as model) =
    div []
        [ global
            [ everything
                [ boxSizing borderBox
                ]
            , html
                [ fontSize (px 16)
                , fontFamilies [ "Nocifer", .value cursive ]
                ]
            , body
                [ fontSize (px 16)
                , fontFamilies [ "Nosifer", .value cursive ]
                , backgroundImage (url background)
                , Css.backgroundSize cover
                , backgroundRepeat noRepeat
                , margin2 zero auto
                , justifyContent center
                , Css.property "display" "grid"
                , Css.property "align-content" "flex-end"
                , Css.width (vw 100)
                , Css.height (vh 100)
                , Css.animationName
                    (keyframes
                        [ ( 0, [ Css.Animations.backgroundColor (rgba 0 0 0 0) ] )
                        , ( 100, [ Css.Animations.backgroundColor (rgba 0 0 0 1) ] )
                        ]
                    )
                , Css.property "animation-duration" "2s"
                ]
            ]
        , countdownView countdown end
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
