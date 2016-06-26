module Main exposing (..)

{-| Show the updated window size
-}

import Html
import Html.App
import Window
import Task.Extra
import Element
import Collage
import Color
import Time
import AnimationFrame
import Keyboard.Extra


type alias Point =
    { x : Float, y : Float }


type alias Ball =
    { location : Point, velocity : Float }


type alias Model =
    { window : Window.Size, ball : Ball }


type Msg
    = Nothing
    | Tick Time.Time
    | KeyboardExtraMsg Keyboard.Extra.Msg
    | WindowSizeChange Window.Size


init : ( Model, Cmd Msg )
init =
    { window = Window.Size -1 -1, ball = { location = { x = 0, y = 0 }, velocity = 0 } } ! [ Window.size |> Task.Extra.performFailproof WindowSizeChange ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nothing ->
            model ! []

        Tick delta ->
            step delta model ! []

        KeyboardExtraMsg keyboardMsg ->
            model ! []

        WindowSizeChange newSize ->
            { model | window = newSize } ! []


step : Float -> Model -> Model
step delta model =
    model
        |> gravity delta
        |> physics delta
        |> collision delta


gravity : Float -> Model -> Model
gravity delta model =
    { model | ball = ballGravity delta model.ball }


ballGravity : Float -> Ball -> Ball
ballGravity delta ball =
    { ball | velocity = ball.velocity - 9.81 * delta }


physics : Float -> Model -> Model
physics delta model =
    { model | ball = ballPhysics delta model.ball }


ballPhysics : Float -> Ball -> Ball
ballPhysics delta ball =
    let
        location =
            ball.location
    in
        { ball | location = { location | y = location.y + ball.velocity * delta * 100 } }


collision : Float -> Model -> Model
collision delta model =
    let
        floorY =
            toFloat model.window.height / -2 + 50
    in
        { model | ball = ballCollision delta floorY model.ball }


ballCollision : Float -> Float -> Ball -> Ball
ballCollision delta floorY ball =
    let
        location =
            ball.location
    in
        if location.y <= floorY then
            { ball | location = { location | y = floorY }, velocity = ball.velocity * -0.9 }
        else
            ball


view : Model -> Html.Html Msg
view model =
    let
        width =
            model.window.width

        height =
            model.window.height
    in
        -- Html.h1 [] [ Html.text ("Window size: " ++ (toString model.window)) ]
        Html.div []
            [ Collage.collage width
                height
                [ Collage.circle 50
                    |> Collage.filled Color.red
                    |> Collage.move ( model.ball.location.x, model.ball.location.y )
                ]
                |> Element.toHtml
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowSizeChange
        , Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
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
