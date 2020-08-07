module Points exposing (..)

type Point
    = Point Int Int

elementSize : Int
elementSize =
    50

getX : Point -> Int
getX (Point x _) =
    x


getY : Point -> Int
getY (Point _ y) =
    y

toPixels : Int -> Int
toPixels x =
    x * elementSize

toPixelPoint : Point -> Point
toPixelPoint p =
    Point (toPixels (getX p)) (toPixels (getY p))

isInside : Point -> Point -> Int -> Bool
isInside towerPos botPos offset =
    isBetween (getX towerPos) (getX botPos) offset && isBetween (getY towerPos) (getY botPos) offset

isBetween : Int -> Int -> Int -> Bool
isBetween x i o =
    i >= toPixels x - o && i <= toPixels x + elementSize + o