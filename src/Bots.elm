module Bots exposing (..)

import Points exposing (Point, getX, getY)
import Points exposing (elementSize)


type alias Bot =
    { position : Point
    , direction : Direction
    , color : String
    , health : Float
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
    toFloat elementSize * 0.8

refreshRate : Float
refreshRate =
    30


spawnRate : Float
spawnRate =
    5000


moveStep : Int
moveStep =
    1


healthPoints : Float
healthPoints =
    10

worth : Int
worth =
    3


healthPointsPercent : Float -> Float
healthPointsPercent h =
    h / healthPoints


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
