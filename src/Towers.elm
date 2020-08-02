module Towers exposing (..)

import Points exposing (Point, elementSize)


type alias Tower =
    { position : Point
    , health : Float
    , color : String
    , attackPoints : List Point
    }


color : String
color =
    "green"

buildCost : Int
buildCost =
    10

repairCost : Int
repairCost =
    8

healthPoints : Float
healthPoints =
    100

healthPointsPercent : Float -> Float
healthPointsPercent h =
    h / healthPoints

range : Int
range =
    round (toFloat elementSize)

damage : Float
damage =
    0.1

attackSpeed : Float
attackSpeed =
    60


create : Point -> Tower
create p =
    { position = p, health = healthPoints, color = color, attackPoints = []}

repair : Point -> List Tower -> List Tower
repair p towers =
    case towers of
        x :: xs ->
            if p == x.position then
                {x | health = healthPoints} :: repair p xs
            else
               x :: repair p xs
        [] ->
            [] 

updateColor : Float -> String
updateColor health =
    if health < healthPoints * 0.3 then
        "red"

    else if health < healthPoints * 0.6 then
        "yellow"

    else
        "green"
