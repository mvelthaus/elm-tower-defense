module Towers exposing (..)

import Points exposing (Point, elementSize)


type alias Tower =
    { position : Point
    , health : Int
    , color : String
    , attackPoints : List Point
    }


color : String
color =
    "green"


healthPoints : Int
healthPoints =
    20

range : Int
range =
    round (toFloat elementSize)

damage : Int
damage =
    1

attackSpeed : Float
attackSpeed =
    100


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

updateColor : Int -> String
updateColor health =
    if health < round (toFloat healthPoints * 0.3) then
        "red"

    else if health < round (toFloat healthPoints * 0.6) then
        "yellow"

    else
        "green"
