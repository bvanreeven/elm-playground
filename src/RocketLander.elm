module RocketLander exposing (..)

{-| Rocket lander!
-}

import AnimationFrame
import Color
import Element exposing (..)
import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Keyboard.Extra
import Time
import Text


type alias Model =
    { rocket : Rocket, keyboard : Keyboard.Extra.Model }


type alias Rocket =
    { x : Float
    , y : Float
    , v : Float
    , thrust : Bool
    }


type Msg
    = Nothing
    | Tick Time.Time
    | Keyboard Keyboard.Extra.Msg


gravity : Float
gravity =
    9.81


init : ( Model, Cmd Msg )
init =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.Extra.init

        model =
            { rocket = Rocket 0 200 0 False, keyboard = keyboardModel }
    in
        model ! [ Cmd.map Keyboard keyboardCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nothing ->
            model ! []

        Tick delta ->
            step delta model ! []

        Keyboard keyMsg ->
            updateKeys keyMsg model


step : Float -> Model -> Model
step delta model =
    let
        newRocket =
            stepRocket delta model.keyboard model.rocket
    in
        { model | rocket = newRocket }


stepRocket : Float -> Keyboard.Extra.Model -> Rocket -> Rocket
stepRocket delta keyboard rocket =
    let
        newThrust =
            Keyboard.Extra.isPressed Keyboard.Extra.ArrowUp keyboard

        newV =
            if newThrust then
                rocket.v - delta * 100
            else
                rocket.v + delta * 50

        newY =
            rocket.y + 4 * delta * newV

        collision =
            newY > 560

        {--
        _ =
            Debug.log "delta" delta

        _ =
            Debug.log "rocket" rocket

        --}
    in
        { rocket
            | v =
                if collision then
                    0
                else
                    newV
            , y =
                if collision then
                    560
                else
                    newY
            , thrust = newThrust
        }


updateKeys : Keyboard.Extra.Msg -> Model -> ( Model, Cmd Msg )
updateKeys keyMsg model =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.Extra.update keyMsg model.keyboard
    in
        { model | keyboard = keyboardModel } ! [ Cmd.map Keyboard keyboardCmd ]


view : Model -> Html.Html Msg
view model =
    let
        rocketImage =
            if model.rocket.thrust then
                "/assets/rocket-thruster-on.png"
            else
                "/assets/rocket-thruster-off.png"

        containerStyle =
            style [ ( "width", "800px" ), ( "height", "600px" ), ( "margin", "10px auto" ) ]
    in
        div [ containerStyle ]
            [ layers
                [ spacer 800 600 |> color Color.black
                , [ "Velocity: " ++ toString (round (model.rocket.v / 10)) |> hudText
                  , "Altitude: " ++ toString (56 - round (model.rocket.y / 10)) |> hudText
                  ]
                    |> flow down
                    |> container 800 600 (topLeftAt (absolute 10) (absolute 10))
                , fittedImage 80 245 rocketImage
                    |> container 800 600 (middleAt (absolute 400) (absolute (round model.rocket.y)))
                ]
                |> toHtml
            ]


hudText : String -> Element
hudText text =
    text
        |> Text.fromString
        |> Text.monospace
        |> Text.color Color.darkGreen
        |> Text.height 32
        |> leftAligned


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Keyboard Keyboard.Extra.subscriptions
        , AnimationFrame.diffs (Tick << Time.inSeconds)
        ]


main : Program Never
main =
    Html.App.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
