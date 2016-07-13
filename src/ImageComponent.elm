module ImageComponent exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Html.Events exposing (..)


type Model
    = Image { imageUrl : String }


type Msg
    = SetImageUrl String
    | Click


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetImageUrl newImageUrl ->
            Image { imageUrl = newImageUrl } ! []

        Click ->
            model ! []


view : Model -> Html Msg
view model =
    case model of
        Image { imageUrl } ->
            img [ src imageUrl, onClick Click ] []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : String -> ( Model, Cmd Msg )
init imageUrl =
    Image { imageUrl = imageUrl } ! []


main : Program Never
main =
    Html.App.program
        { init = init "https://media.giphy.com/media/o0vwzuFwCGAFO/giphy.gif"
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
