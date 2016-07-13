module ShowImages exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import ImageComponent


type alias Model =
    List IndexedImageComponent


type alias IndexedImageComponent =
    { id : Int, model : ImageComponent.Model }


type Msg
    = Noop
    | ImageComponentMsg Int ImageComponent.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


view : Model -> Html Msg
view model =
    let
        viewIndexedImageComponent : IndexedImageComponent -> Html Msg
        viewIndexedImageComponent { id, model } =
            Html.App.map (ImageComponentMsg id) (ImageComponent.view model)
    in
        div []
            (model |> List.map (\x -> (div [] [ viewIndexedImageComponent x ])))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    let
        createIndexedImageComponent : Int -> String -> IndexedImageComponent
        createIndexedImageComponent id url =
            IndexedImageComponent id (fst (ImageComponent.init url))

        -- disregarding messages now, not nice
    in
        ([ "https://media.giphy.com/media/o0vwzuFwCGAFO/giphy.gif"
         , "https://media.giphy.com/media/lE5u6gdLEXA9W/giphy.gif"
         , "https://media.giphy.com/media/26FPGsG2NZL1QzAFW/giphy.gif"
         ]
            |> List.indexedMap createIndexedImageComponent
        )
            ! []


main : Program Never
main =
    Html.App.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
