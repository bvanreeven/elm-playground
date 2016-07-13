module ShowTime exposing (..)

import Html.App exposing (program)
import Html exposing (..)
import Time exposing (Time)
import Date exposing (..)
import Task exposing (Task)
import Date.Format


type alias Model =
    Time


type Msg
    = Tick Time


init : ( Model, Cmd Msg )
init =
    0 ! [ Task.perform never Tick Time.now ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            time ! []


view : Model -> Html Msg
view model =
    let
        date =
            Date.fromTime model

        format =
            Date.Format.format "%Y-%m-%d %H:%M:%S"
    in
        h1 [] [ text <| format date ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (0.1 * Time.second) Tick


main : Program Never
main =
    Html.App.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = (view)
        }


never : Never -> a
never n =
    never n
