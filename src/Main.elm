module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, p, section, text)
import Html.Attributes exposing (class, src)
import Task
import Time exposing (Month(..), utc)
import Time.Extra as Time



---- MODEL ----


type alias Model =
    { zone : Time.Zone
    , posix : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { zone = Time.utc, posix = Time.millisToPosix 0 }, setSystemTime )



---- UPDATE ----


type Msg
    = SetSystemTime ( Time.Zone, Time.Posix )
    | SetCurrentTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSystemTime ( zone, time ) ->
            ( { zone = zone, posix = time }, Cmd.none )

        SetCurrentTime time ->
            ( { model | posix = time }, Cmd.none )



---- VIEW ----


gyozaDay : Time.Posix
gyozaDay =
    Time.partsToPosix utc (Time.Parts 2018 Time.Oct 22 8 0 0 0)


diffGyozaDay : Time.Posix -> Time.Interval -> Int
diffGyozaDay tm interval =
    Time.diff interval utc tm gyozaDay


view : Model -> Html Msg
view model =
    let
        p =
            model.posix

        d =
            diffGyozaDay p Time.Day

        h =
            diffGyozaDay p Time.Hour - (d * 24)

        m =
            diffGyozaDay p Time.Minute - (d * 24 + h) * 60

        s =
            diffGyozaDay p Time.Second - ((d * 24 + h) * 60 + m) * 60
    in
    section [ class "hero is-fullheight is-dark" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ div [ class "columns is-mobile" ]
                    [ div [ class "column" ]
                        [ h1 [ class "title is-3" ]
                            [ text "Gyoza" ]
                        ]
                    ]
                , div [ class "columns is-mobile" ]
                    [ timeElement (String.fromInt d)
                    , timeElement (String.fromInt h)
                    , timeElement (String.fromInt m)
                    , timeElement (String.fromInt s)
                    ]
                , div [ class "columns is-mobile" ]
                    [ unitElement "Days"
                    , unitElement "Hours"
                    , unitElement "Mins"
                    , unitElement "Sec"
                    ]
                ]
            ]
        ]


timeElement : String -> Html Msg
timeElement tm =
    div [ class "column" ]
        [ p [ class "is-size-2" ]
            [ text tm ]
        ]


unitElement : String -> Html Msg
unitElement unit =
    div [ class "column" ]
        [ text unit ]


setSystemTime : Cmd Msg
setSystemTime =
    Task.perform SetSystemTime <| Task.map2 Tuple.pair Time.here Time.now


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 SetCurrentTime


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
