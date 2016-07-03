module Ball exposing (..)

import Color
import Point exposing (Point)
import Collage


type alias Ball =
    { location : Point, velocity : Float, radius : Float, color : Color.Color }


init : Float -> Point -> Color.Color -> Ball
init radius location color =
    { radius = radius, location = location, velocity = 0, color = color }


update : Float -> Float -> Ball -> Ball
update delta floorY ball =
    ball
        |> gravity delta
        |> physics delta
        |> collision delta floorY


gravity : Float -> Ball -> Ball
gravity delta ball =
    { ball | velocity = ball.velocity - 9.81 * delta }


physics : Float -> Ball -> Ball
physics delta ball =
    let
        location =
            ball.location
    in
        { ball | location = { location | y = location.y + ball.velocity * delta * 100 } }


collision : Float -> Float -> Ball -> Ball
collision delta floorY ball =
    let
        location =
            ball.location

        bottomY =
            location.y - ball.radius
    in
        if bottomY <= floorY then
            { ball | location = { location | y = floorY + ball.radius }, velocity = ball.velocity * -0.9 }
        else
            ball


view : Ball -> Collage.Form
view ball =
    Collage.circle ball.radius
        |> Collage.filled ball.color
        |> Collage.move ( ball.location.x, ball.location.y )
