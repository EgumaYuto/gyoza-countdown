module Main exposing (..)

import Html exposing (Html, div, p, text, program)
import Html.Attributes exposing (..)
import Hello

-- Model
type alias Model =
    String

init : ( Model, Cmd Msg )
init =
    ( Hello.hello, Cmd.none )


-- Message
type Msg
    = NoOp


-- View
view : Model -> Html Msg
view model =
    p [class "msg"] [text model]


-- Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- Main
main : Program Never Model Msg
main =
    program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
