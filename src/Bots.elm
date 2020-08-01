module Bots exposing (..)

import Points exposing (Point, getX, getY)

type alias Bot =
    {position : Point
    , direction : Direction
    , color : String
    , health : Int
    , collided : Bool}

type Direction
    = Right
    | Left
    | Up
    | Down
    | Stop

refreshRate : Float
refreshRate =
    40


spawnRate : Float
spawnRate =
    5000

moveStep : Int
moveStep =
    1

type alias MoveArea
    = List Point

initMoveArea : Int -> Int -> MoveArea
initMoveArea length y=
    if length > 0 then
        Points.Point length y :: initMoveArea (length-1) y
    else
        []


createBot : Point -> Bot
createBot start =
    {position = start, direction = Right, color = "black", health = 10, collided = False}


move : Bot -> Bot
move bot =
    case bot.direction of
        Right ->
            {bot | position = Points.Point (getX bot.position + moveStep) (getY bot.position)}
        Left ->
            {bot | position = Points.Point (getX bot.position - moveStep) (getY bot.position)}
        Up ->
            {bot | position = Points.Point (getX bot.position) (getY bot.position - moveStep)}
        Down ->
            {bot | position = Points.Point (getX bot.position) (getY bot.position + moveStep)}
        Stop ->
            bot