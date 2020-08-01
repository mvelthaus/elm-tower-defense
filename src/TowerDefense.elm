module TowerDefense exposing (main)

import Bots exposing (Bot, Direction, createBot, move)
import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Lists exposing (any)
import Points exposing (Point, getX, getY)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Time exposing (every)


type alias PitchElement =
    { position : Point
    , color : String
    }


botSpeed : Float
botSpeed =
    30


spawnRate : Float
spawnRate =
    10000


spawnPoint : Point
spawnPoint =
    Points.Point 0 (height * elementSize // 2)


collidingOffset : Int
collidingOffset =
    4


type Msg
    = Click Point
    | MouseOver Point
    | MouseOut Point
    | Tick
    | Spawn


type State
    = Running
    | Lost
    | Paused


type alias Playground =
    List PitchElement


type alias Model =
    { selectionLayer : Playground
    , towerLayer : Playground
    , cash : Int
    , health : Int
    , bots : List Bots.Bot
    , state : State
    }


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { selectionLayer = buildPitch width height, towerLayer = [], cash = 1000, health = 10, bots = [ createBot spawnPoint ], state = Running }, Cmd.none )


buildPitch : Int -> Int -> Playground
buildPitch w h =
    let
        buildRows row =
            if row >= 0 then
                buildRow w row ++ buildRows (row - 1)

            else
                []
    in
    buildRows h


buildRow : Int -> Int -> List PitchElement
buildRow column row =
    if column >= 0 then
        { position = Points.Point column row, color = defaultColor } :: buildRow (column - 1) row

    else
        []


elementSize : Int
elementSize =
    10


width : Int
width =
    30


widthInPixels : Int
widthInPixels =
    toPixels width


height : Int
height =
    20


heightInPixels : Int
heightInPixels =
    toPixels height


toPixels : Int -> Int
toPixels x =
    x * elementSize


defaultColor : String
defaultColor =
    "lightgrey"


hoverColor : String
hoverColor =
    "grey"


towerColor : String
towerColor =
    "red"


moveBots : List Bot -> Playground -> List Bot
moveBots bots towers =
    case bots of
        x :: xs ->
            if collisionEnd x then
                moveBots xs towers

            else
                move (collision x towers) :: moveBots xs towers

        [] ->
            []


collision : Bot -> Playground -> Bot
collision bot towers =
    if towerCollision bot towers then
        if bot.collided then
            if getY bot.position <= 0 + collidingOffset then
                { bot | direction = Bots.Down }

            else if getY bot.position >= heightInPixels - collidingOffset then
                { bot | direction = Bots.Up }

            else
                bot

        else if getY bot.position > heightInPixels // 2 then
            { bot | direction = Bots.Up, collided = True }

        else
            { bot | direction = Bots.Down, collided = True }

    else
        { bot | direction = Bots.Right, collided = False }


collisionEnd : Bot -> Bool
collisionEnd b =
    getX b.position > widthInPixels


towerCollision : Bot -> Playground -> Bool
towerCollision bot towers =
    Lists.any (\p -> isInside p.position bot.position) towers


isInside : Point -> Point -> Bool
isInside towerPos botPos =
    isBetween (getX towerPos) (getX botPos) && isBetween (getY towerPos) (getY botPos)


isBetween : Int -> Int -> Bool
isBetween x i =
    i >= toPixels x - collidingOffset && i <= toPixels x + elementSize + collidingOffset


borderCollision : Bot -> Direction
borderCollision bot =
    if getY bot.position <= 0 then
        Bots.Down

    else if getY bot.position >= heightInPixels then
        Bots.Up

    else
        bot.direction


mark : Point -> String -> List PitchElement -> List PitchElement
mark p c l =
    case l of
        x :: xs ->
            if x.position == p then
                { x | color = c } :: mark p c xs

            else
                x :: mark p c xs

        [] ->
            []


createTower : Point -> List PitchElement -> List PitchElement
createTower p l =
    case l of
        x :: xs ->
            if x.position == p then
                { x | color = towerColor } :: createTower p xs

            else
                createTower p xs

        [] ->
            []


pitchToSvg : Playground -> Playground -> List Bot -> List (Svg Msg)
pitchToSvg selection tower bot =
    List.map drawTower tower ++ List.map drawBot bot ++ List.map drawRect selection


drawRect : PitchElement -> Svg Msg
drawRect { position, color } =
    rect
        [ Svg.Events.onClick (Click position)
        , Svg.Events.onMouseOut (MouseOut position)
        , Svg.Events.onMouseOver (MouseOver position)
        , x (String.fromInt (getX position * elementSize))
        , y (String.fromInt (getY position * elementSize))
        , Svg.Attributes.width (String.fromInt elementSize)
        , Svg.Attributes.height (String.fromInt elementSize)
        , fill color
        , fillOpacity "0.3"
        ]
        []


drawTower : PitchElement -> Svg Msg
drawTower { position, color } =
    rect
        [ x (String.fromInt (getX position * elementSize))
        , y (String.fromInt (getY position * elementSize))
        , Svg.Attributes.width (String.fromInt elementSize)
        , Svg.Attributes.height (String.fromInt elementSize)
        , fill color
        ]
        []


drawBot : Bot -> Svg Msg
drawBot { position, color } =
    circle
        [ cx (String.fromInt (getX position))
        , cy (String.fromInt (getY position))
        , Svg.Attributes.r (String.fromFloat (toFloat elementSize * 0.2))
        , fill color
        ]
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pos ->
            if model.cash >= 10 && Lists.any (\p -> pos == p.position && p.color == towerColor) model.towerLayer == False then
                ( { model | towerLayer = model.towerLayer ++ createTower pos model.selectionLayer, cash = model.cash - 10 }, Cmd.none )

            else
                ( model, Cmd.none )

        MouseOver pos ->
            ( { model | selectionLayer = mark pos hoverColor model.selectionLayer }, Cmd.none )

        MouseOut pos ->
            ( { model | selectionLayer = mark pos defaultColor model.selectionLayer }, Cmd.none )

        Tick ->
            if Lists.any (\p -> getX p.position > widthInPixels) model.bots then
                ( { model | health = model.health - 1, bots = moveBots model.bots model.towerLayer }, Cmd.none )

            else
                ( { model | bots = moveBots model.bots model.towerLayer }, Cmd.none )

        Spawn ->
            ( { model | bots = createBot spawnPoint :: model.bots }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ Html.Attributes.style "width" "60%", Html.Attributes.style "margin-right" "auto", Html.Attributes.style "margin-left" "auto" ]
        [ div [ Html.Attributes.style "height" "250px" ]
            [ {- Html.button [ onClick (Click 5) ] [ Html.text "Up" ] -}
              -- , Html.button [ onClick (Dir Left) ] [ Html.text "Left" ]
              -- , Html.button [ onClick (Dir Right) ] [ Html.text "Right" ]
              -- , Html.button [ onClick (Dir Down) ] [ Html.text "Down" ]
              --, Html.button [ onClick Extend ] [ Html.text "Extend" ]
              Html.text ("Cash: " ++ String.fromInt model.cash)
            , Html.text (" Health: " ++ String.fromInt model.health)
            , div []
                [ svg
                    [ viewBox (String.concat [ "0 0 ", String.fromInt (width * elementSize), " ", String.fromInt (height * elementSize) ])
                    ]
                    (pitchToSvg model.selectionLayer model.towerLayer model.bots)
                ]
            ]

        -- , setMessage model.state
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every botSpeed (\_ -> Tick), Time.every spawnRate (\_ -> Spawn) ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
