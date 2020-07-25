module Points exposing (..)

type Point
    = Point Int Int

getX : Point -> Int
getX (Point x _) =
    x


getY : Point -> Int
getY (Point _ y) =
    y