module TowerDefense exposing (main)

import Bots exposing (Bot, createBot, move)
import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List
import Lists
import Points exposing (Point, elementSize, getX, getY, isBetween, isInside, toPixels)
import Selections exposing (PitchElement)
import String exposing (fromInt, fromFloat)
import Svg exposing (Svg)
import Svg.Attributes exposing (cx, cy, fill, fillOpacity, viewBox, x, y)
import Svg.Events exposing (onClick)
import Time exposing (every)
import Towers exposing (Tower, create)


spawnPoint : Point
spawnPoint =
    Points.Point 0 (height * elementSize // 2)


collidingOffset : Int
collidingOffset =
    round (toFloat elementSize * 0.4)


type Msg
    = Click Point
    | MouseOver Point
    | MouseOut Point
    | Tick
    | Spawn
    | Attack


type State
    = Running
    | Lost
    | Paused


type alias Model =
    { selectionLayer : List PitchElement
    , towers : List Tower
    , cash : Int
    , health : Int
    , bots : List Bots.Bot
    , state : State
    }


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { selectionLayer = buildPitch width height, towers = [], cash = 1000, health = 10, bots = [ createBot spawnPoint ], state = Running }, Cmd.none )


buildPitch : Int -> Int -> List PitchElement
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


width : Int
width =
    20


widthInPixels : Int
widthInPixels =
    toPixels width


height : Int
height =
    10


heightInPixels : Int
heightInPixels =
    toPixels height


defaultColor : String
defaultColor =
    "lightgrey"


hoverColor : String
hoverColor =
    "grey"


moveBots : List Bot -> List Tower -> List Bot
moveBots bots towers =
    case bots of
        x :: xs ->
            if collisionEnd x then
                moveBots xs towers

            else
                move (collision towers x) :: moveBots xs towers

        [] ->
            []


collision : List Tower -> Bot -> Bot
collision towers bot =
    if getY bot.position <= 0 + collidingOffset then
        { bot | direction = Bots.Down }

    else if getY bot.position >= heightInPixels - collidingOffset then
        { bot | direction = Bots.Up }

    else
        collisionTower bot towers


collisionEnd : Bot -> Bool
collisionEnd b =
    getX b.position > widthInPixels


collisionTower : Bot -> List Tower -> Bot
collisionTower bot towers =
    if Lists.any (\p -> collideYBot p.position bot.position) towers then
        { bot | direction = Bots.Down }

    else if Lists.any (\p -> collideYTop p.position bot.position) towers then
        { bot | direction = Bots.Up }

    else if Lists.any (\p -> collideX p.position bot.position) towers then
        if bot.collided == False then
            if getY bot.position > heightInPixels // 2 then
                { bot | direction = Bots.Up, collided = True }

            else
                { bot | direction = Bots.Down, collided = True }

        else
            bot

    else
        { bot | direction = Bots.Right, collided = False }


attackBots : List Bot -> List Tower -> List Bot
attackBots bots towers =
    Lists.doubleMap (\p -> p.health <= 0) updateBotHealth bots towers


attackTowers : List Tower -> List Bot -> List Tower
attackTowers towers bots =
    Lists.doubleMap (\p -> p.health <= 0) updateTowerHealth towers bots


updateBotHealth : List Tower -> Bot -> Bot
updateBotHealth towers bot =
    if Lists.any (\p -> isInside p.position bot.position Towers.range) towers then
        { bot | health = bot.health - 1 }

    else
        bot


updateTowerHealth : List Bot -> Tower -> Tower
updateTowerHealth bots tower =
    if Lists.any (\p -> isInside tower.position p.position Towers.range) bots then
        { tower | health = tower.health - 1, {- color = Towers.updateColor tower.health, -} attackPoints = addAttackPoints bots tower }

    else
        { tower | attackPoints = [] }


addAttackPoints : List Bot -> Tower -> List Point
addAttackPoints bots tower =
    case bots of
        x :: xs ->
            if isInside tower.position x.position Towers.range then
                x.position :: addAttackPoints xs tower

            else
                addAttackPoints xs tower

        [] ->
            []


collideX : Point -> Point -> Bool
collideX towerPos botPos =
    toPixels (getX towerPos) - collidingOffset == getX botPos && isBetween (getY towerPos) (getY botPos) collidingOffset


collideYBot : Point -> Point -> Bool
collideYBot towerPos botPos =
    isBetween (getX towerPos) (getX botPos) 0 && toPixels (getY towerPos) + elementSize + collidingOffset == getY botPos


collideYTop : Point -> Point -> Bool
collideYTop towerPos botPos =
    isBetween (getX towerPos) (getX botPos) 0 && toPixels (getY towerPos) - collidingOffset == getY botPos


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


pitchToSvg : List PitchElement -> List Tower -> List Bot -> List (Svg Msg)
pitchToSvg selection tower bot =
    List.map drawTower tower ++ List.map drawBot bot ++ List.map drawRect selection


drawRect : PitchElement -> Svg Msg
drawRect { position, color } =
    Svg.rect
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


drawAttackLine : Point -> Point -> Svg Msg
drawAttackLine towerPos attackPos =
    Svg.line
        [ Svg.Attributes.x1 (fromInt (toPixels (getX towerPos) + elementSize // 2))
        , Svg.Attributes.y1 (fromInt (toPixels (getY towerPos) + elementSize // 2))
        , Svg.Attributes.x2 (fromInt (getX attackPos))
        , Svg.Attributes.y2 (fromInt (getY attackPos))
        , Svg.Attributes.style "stroke:rgb(0,0,255);stroke-width:1"
        ]
        []


drawAttackLines : List Point -> Point -> List (Svg Msg)
drawAttackLines attackPoints towerPos =
    case attackPoints of
        x :: xs ->
            drawAttackLine towerPos x :: drawAttackLines xs towerPos

        [] ->
            []


drawTower : Tower -> Svg Msg
drawTower tower =
    Svg.g
        []
        (drawAttackLines tower.attackPoints tower.position
            ++ [ Svg.rect
                    [ x (String.fromInt (getX tower.position * elementSize))
                    , y (String.fromInt (getY tower.position * elementSize))
                    , Svg.Attributes.width (String.fromInt elementSize)
                    , Svg.Attributes.height (String.fromInt elementSize)
                    , fill (Towers.updateColor tower.health)
                    ]
                    []
               ]
        )


drawBot : Bot -> Svg Msg
drawBot { position, color, health } =
    Svg.g
        []
        [ Svg.rect
            [ x (fromInt (getX position - round (Bots.size/2)))
            , y (fromInt (getY position - 8))
            , Svg.Attributes.width (fromFloat (Bots.size * Bots.healthPointsPercent health))
            , Svg.Attributes.height (fromFloat (toFloat elementSize * 0.1))
            , fill "green"
            ]
            []
        , Svg.circle
            [ cx (fromInt (getX position))
            , cy (fromInt (getY position))
            , Svg.Attributes.r (fromFloat (Bots.size/2))
            , fill color
            ]
            []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pos ->
            if model.cash >= 10 && Lists.any (\p -> pos == p.position) model.towers == False then
                ( { model | towers = create pos :: model.towers, cash = model.cash - 10 }, Cmd.none )
            else if model.cash >= 5 && Lists.any (\p -> pos == p.position) model.towers then
                ( { model | towers = Towers.repair pos model.towers, cash = model.cash - 5 }, Cmd.none )
            else
                ( model, Cmd.none )

        MouseOver pos ->
            ( { model | selectionLayer = mark pos hoverColor model.selectionLayer }, Cmd.none )

        MouseOut pos ->
            ( { model | selectionLayer = mark pos defaultColor model.selectionLayer }, Cmd.none )

        Tick ->
            if Lists.any (\p -> getX p.position > widthInPixels) model.bots then
                ( { model | health = model.health - 1, bots = moveBots model.bots model.towers }, Cmd.none )

            else
                ( { model | bots = moveBots model.bots model.towers }, Cmd.none )

        Spawn ->
            ( { model | bots = createBot spawnPoint :: model.bots }, Cmd.none )

        Attack ->
            if Lists.any (\p -> p.health <= 0) model.bots then
                ( { model | cash = model.cash + 5, bots = attackBots model.bots model.towers, towers = attackTowers model.towers model.bots }, Cmd.none )

            else
                ( { model | bots = attackBots model.bots model.towers, towers = attackTowers model.towers model.bots }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ Html.Attributes.style "width" "80%", Html.Attributes.style "margin-right" "auto", Html.Attributes.style "margin-left" "auto" ]
        [ div [ Html.Attributes.style "height" "250px" ]
            [ {- Html.button [ onClick (Click 5) ] [ Html.text "Up" ] -}
              -- , Html.button [ onClick (Dir Left) ] [ Html.text "Left" ]
              -- , Html.button [ onClick (Dir Right) ] [ Html.text "Right" ]
              -- , Html.button [ onClick (Dir Down) ] [ Html.text "Down" ]
              --, Html.button [ onClick Extend ] [ Html.text "Extend" ]
              Html.text ("Cash: " ++ String.fromInt model.cash)
            , Html.text (" Health: " ++ String.fromInt model.health)
            , div []
                [ Svg.svg
                    [ viewBox (String.concat [ "0 0 ", String.fromInt (width * elementSize), " ", String.fromInt (height * elementSize) ])
                    ]
                    (pitchToSvg model.selectionLayer model.towers model.bots)
                ]
            ]

        -- , setMessage model.state
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Running ->
            Sub.batch [ Time.every Bots.refreshRate (\_ -> Tick), Time.every Bots.spawnRate (\_ -> Spawn), Time.every Towers.attackSpeed (\_ -> Attack) ]

        Paused ->
            Sub.none

        Lost ->
            Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
