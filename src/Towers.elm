module Towers exposing (..)

import Points exposing (Point, elementSize, getX, getY)


type alias Tower =
    { position : Point
    , health : Float
    , color : String
    , attackPoint : Maybe Point
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
    { position = p, health = healthPoints, color = color, attackPoint = Nothing }


repair : Point -> List Tower -> List Tower
repair p towers =
    case towers of
        x :: xs ->
            if p == x.position then
                { x | health = healthPoints } :: repair p xs

            else
                x :: repair p xs

        [] ->
            []

delete : Point -> List Tower -> List Tower
delete p towers =
    case towers of
        x :: xs ->
            if p == x.position then
                delete p xs

            else
                x :: delete p xs

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


getRotation : Maybe Point -> Point -> String
getRotation attackPoint pos =
    case attackPoint of
        Just p ->
            "rotate(" ++ String.fromFloat (90+toDegrees (atan2 (toFloat (getY p - (getY pos+elementSize//2))) (toFloat (getX p - (getX pos+elementSize//2))))) ++ "," ++ getRotationPoint pos ++ ")"

        Nothing ->
            "rotate(270," ++ getRotationPoint pos ++ ")"


toDegrees : Float -> Float
toDegrees r =
    r * 180 / pi


getRotationPoint : Point -> String
getRotationPoint pos =
    String.fromInt (getX pos + elementSize // 2) ++ "," ++ String.fromInt (getY pos + elementSize // 2)
