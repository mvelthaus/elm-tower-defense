-- Author: Mirko Velthaus


module Towers exposing (Tower, attackSpeed, buildCost, calculateDamage, create, delete, getRotation, healthPointsPercent, range, repair, repairCost, resetAttackPoint, updateColor)

import Points exposing (Point, elementSize, getX, getY)


type alias Tower =
    { position : Point
    , damage : Float
    , attackPoint : Maybe Point
    }


buildCost : Int
buildCost =
    10


repairCost : Int
repairCost =
    5


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
    1


damageDecrease : Float
damageDecrease =
    0.002


calculateDamage : Float -> Float
calculateDamage dmg =
    if dmg <= 0 then
        0

    else
        dmg - damageDecrease


attackSpeed : Float
attackSpeed =
    60


create : Point -> Tower
create p =
    { position = p, damage = damage, attackPoint = Nothing }


repair : Point -> List Tower -> List Tower
repair p towers =
    case towers of
        x :: xs ->
            if p == x.position then
                { x | damage = damage } :: repair p xs

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
    if health < damage * 0.3 then
        "red"

    else if health < damage * 0.6 then
        "yellow"

    else
        "green"


getRotation : Maybe Point -> Point -> String
getRotation attackPoint pos =
    case attackPoint of
        Just p ->
            "rotate(" ++ String.fromFloat (90 + toDegrees (atan2 (toFloat (getY p - (getY pos + elementSize // 2))) (toFloat (getX p - (getX pos + elementSize // 2))))) ++ "," ++ getRotationPoint pos ++ ")"

        Nothing ->
            "rotate(270," ++ getRotationPoint pos ++ ")"


toDegrees : Float -> Float
toDegrees r =
    r * 180 / pi


getRotationPoint : Point -> String
getRotationPoint pos =
    String.fromInt (getX pos + elementSize // 2) ++ "," ++ String.fromInt (getY pos + elementSize // 2)


resetAttackPoint : List Tower -> List Tower
resetAttackPoint towers =
    List.map (\p -> { p | attackPoint = Nothing }) towers
