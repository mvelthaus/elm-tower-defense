-- Author: Mirko Velthaus


module Bots exposing (Bot, BotType(..), Direction(..), botTypeGenerator, createBot, directionGenerator, getImageLink, getRoation, healthPointsPercent, move, size, spawnRate, worth)

import Points exposing (Point(..), elementSize, getX, getY)
import Random
import String


type alias Bot =
    { position : Point
    , direction : Direction
    , color : String
    , health : Float
    , collided : Bool
    , botType : BotType
    }


type BotType
    = Default
    | Boss


type Direction
    = Right
    | Up
    | Down
    | Stop


size : Float
size =
    toFloat elementSize



-- time that passes between bot spawns calculated on the number of bots already spawned; minimum = 200 ms


spawnRate : Int -> Float
spawnRate spawned =
    if (5 / (1 + toFloat (spawned // 7)) * 2000) > 200 then
        5 / (1 + toFloat (spawned // 7)) * 2000

    else
        200


moveStep : Int
moveStep =
    1


healthPoints : Float
healthPoints =
    100


worth : Int
worth =
    5


directionGenerator : Random.Generator Direction
directionGenerator =
    Random.uniform Up [ Down ]


botTypeGenerator : Random.Generator BotType
botTypeGenerator =
    Random.weighted ( 65, Default ) [ ( 35, Boss ) ]


healthPointsPercent : BotType -> Float -> Float
healthPointsPercent botType h =
    h / getHealth botType


createBot : Point -> BotType -> Bot
createBot start botType =
    { position = start, direction = Right, color = "black", health = getHealth botType, collided = False, botType = botType }


move : Bot -> Bot
move bot =
    case bot.direction of
        Right ->
            { bot | position = Point (getX bot.position + moveStep) (getY bot.position) }

        Up ->
            { bot | position = Point (getX bot.position) (getY bot.position - moveStep) }

        Down ->
            { bot | position = Point (getX bot.position) (getY bot.position + moveStep) }

        Stop ->
            bot


getImageLink : BotType -> String
getImageLink botType =
    case botType of
        Default ->
            "Graphics/default_bot.png"

        Boss ->
            "Graphics/boss_bot.png"


getHealth : BotType -> Float
getHealth botType =
    case botType of
        Default ->
            healthPoints

        Boss ->
            healthPoints * 2


getRoation : Direction -> Point -> String
getRoation direct pos =
    case direct of
        Right ->
            "rotate(0)"

        Up ->
            "rotate(-90," ++ String.fromInt (getX pos) ++ "," ++ String.fromInt (getY pos) ++ ")"

        Down ->
            "rotate(90," ++ String.fromInt (getX pos) ++ "," ++ String.fromInt (getY pos) ++ ")"

        Stop ->
            "rotate(0)"
