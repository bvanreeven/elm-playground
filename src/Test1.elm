module Main exposing (..)

{-| Bouncing balls!
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
import Mouse
import Ball exposing (Ball)
import Point exposing (Point)


type alias Model =
    { window : Window.Size, balls : List Ball, keyboard : Keyboard.Extra.Model }


type Msg
    = Nothing
    | Tick Time.Time
    | KeyboardExtraMsg Keyboard.Extra.Msg
    | MouseClick Mouse.Position
    | WindowSizeChange Window.Size


init : ( Model, Cmd Msg )
init =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.Extra.init

        model =
            { window = Window.Size -1 -1, balls = [ Ball.init 50 { x = 0, y = 0 } Color.red ], keyboard = keyboardModel }
    in
        model ! [ Window.size |> Task.Extra.performFailproof WindowSizeChange, Cmd.map KeyboardExtraMsg keyboardCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nothing ->
            model ! []

        Tick delta ->
            step delta model ! []

        KeyboardExtraMsg keyMsg ->
            updateKeys keyMsg model

        MouseClick position ->
            { model | balls = Ball.init 50 (mousePositionToPoint model.window position) (nextColor position) :: model.balls } ! []

        WindowSizeChange newSize ->
            { model | window = newSize } ! []


updateKeys : Keyboard.Extra.Msg -> Model -> ( Model, Cmd Msg )
updateKeys keyMsg model =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.Extra.update keyMsg model.keyboard
    in
        { model | keyboard = keyboardModel } ! [ Cmd.map KeyboardExtraMsg keyboardCmd ]


nextColor : Mouse.Position -> Color.Color
nextColor pos =
    case pos.x % 3 of
        0 ->
            Color.red

        1 ->
            Color.green

        _ ->
            Color.blue


mousePositionToPoint : Window.Size -> Mouse.Position -> Point
mousePositionToPoint window position =
    { x = toFloat position.x - toFloat window.width / 2, y = toFloat -position.y + toFloat window.height / 2 }


step : Float -> Model -> Model
step delta model =
    model
        |> gravity delta
        |> physics delta
        |> collision delta


gravity : Float -> Model -> Model
gravity delta model =
    { model | balls = model.balls |> List.map (Ball.gravity delta) }


physics : Float -> Model -> Model
physics delta model =
    { model | balls = model.balls |> List.map (Ball.physics delta) }


collision : Float -> Model -> Model
collision delta model =
    let
        floorY =
            toFloat model.window.height / -2
    in
        { model | balls = model.balls |> List.map (Ball.collision delta floorY) }


view : Model -> Html.Html Msg
view model =
    let
        width =
            model.window.width

        height =
            model.window.height
    in
        Html.div []
            [ model.balls
                |> List.map Ball.view
                |> Collage.collage width height
                |> Element.toHtml
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowSizeChange
        , Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
        , Mouse.clicks MouseClick
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
