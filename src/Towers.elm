module Towers exposing (..)

import Bots exposing (Bot)
import Points exposing (Point, isInside)
import Lists exposing (any)


type alias Tower =
    { position : Point
    , health : Int
    , color : String
    }


color : String
color =
    "green"


healthPoints : Int
healthPoints =
    100

range : Int
range =
    6


create : Point -> Tower
create p =
    { position = p, health = healthPoints, color = color }

updateHealth : List Bot -> List Tower -> List Tower
updateHealth bots towers =
    case towers of
        x :: xs ->
            if x.health <= 0 then
                updateHealth bots xs
            else
                collision bots x :: updateHealth bots xs

        [] ->
            towers

collision : List Bot -> Tower -> Tower
collision bots tower =
    if Lists.any (\p -> isInside tower.position p.position range) bots then
        {tower | health = tower.health - 1, color = updateColor tower.health}
    else
        tower


updateColor : Int -> String
updateColor health =
    if health < round (toFloat healthPoints * 0.3) then
        "red"

    else if health < round (toFloat healthPoints * 0.6) then
        "yellow"

    else
        "green"
