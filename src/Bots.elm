module Bots exposing (..)

import Points exposing (Point, getX, getY)
import Points exposing (elementSize)
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
    toFloat elementSize {- * 0.8 -}

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

directionGenerator : Random.Generator Direction
directionGenerator =
    Random.uniform Up [Down]

botTypeGenerator : Random.Generator BotType
botTypeGenerator =
    Random.weighted (80, Default) [(20, Boss)]


healthPointsPercent : BotType -> Float -> Float
healthPointsPercent botType h =
    h / getHealth botType


type alias MoveArea =
    List Point


createBot : Point -> BotType -> Bot
createBot start botType =
    { position = start, direction = Right, color = "black", health = getHealth botType, collided = False, botType = botType }


move : Bot -> Bot
move bot =
    case bot.direction of
        Right ->
            { bot | position = Points.Point (getX bot.position + moveStep) (getY bot.position) }

        Up ->
            { bot | position = Points.Point (getX bot.position) (getY bot.position - moveStep) }

        Down ->
            { bot | position = Points.Point (getX bot.position) (getY bot.position + moveStep) }

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
getRoation direct pos=
    case direct of
        Right ->
            "rotate(0)"
        Up ->
            "rotate(-90,"++String.fromInt (getX pos)++","++String.fromInt (getY pos)++")"
        Down ->
            "rotate(90,"++String.fromInt (getX pos)++","++String.fromInt (getY pos)++")"
        Stop ->
            "rotate(0)"
        