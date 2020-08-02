module Bots exposing (..)

import Points exposing (Point, getX, getY)
import Points exposing (elementSize)


type alias Bot =
    { position : Point
    , direction : Direction
    , color : String
    , health : Int
    , collided : Bool
    }


type Direction
    = Right
    | Left
    | Up
    | Down
    | Stop

size : Float
size =
    toFloat elementSize * 0.6

refreshRate : Float
refreshRate =
    30


spawnRate : Float
spawnRate =
    5000


moveStep : Int
moveStep =
    1


healthPoints : Int
healthPoints =
    10


healthPointsPercent : Int -> Float
healthPointsPercent h =
    toFloat h / toFloat healthPoints


type alias MoveArea =
    List Point


createBot : Point -> Bot
createBot start =
    { position = start, direction = Right, color = "black", health = healthPoints, collided = False }


move : Bot -> Bot
move bot =
    case bot.direction of
        Right ->
            { bot | position = Points.Point (getX bot.position + moveStep) (getY bot.position) }

        Left ->
            { bot | position = Points.Point (getX bot.position - moveStep) (getY bot.position) }

        Up ->
            { bot | position = Points.Point (getX bot.position) (getY bot.position - moveStep) }

        Down ->
            { bot | position = Points.Point (getX bot.position) (getY bot.position + moveStep) }

        Stop ->
            bot



-- move : Bot -> Point
-- move {position, direction} =
--     case direction of
--         Right ->
--             Points.Point (getX position + moveStep) (getY position)
--         Left ->
--             Points.Point (getX position - moveStep) (getY position)
--         Up ->
--             Points.Point (getX position) (getY position - moveStep)
--         Down ->
--             Points.Point (getX position) (getY position + moveStep)
--         Stop ->
--             position
