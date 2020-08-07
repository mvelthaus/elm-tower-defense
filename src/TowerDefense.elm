module TowerDefense exposing (main)

import Bots exposing (Bot, Direction, createBot, move)
import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List
import Lists
import Points exposing (Point, elementSize, getX, getY, isBetween, isInside, toPixelPoint, toPixels)
import Random
import Selections exposing (PitchElement)
import String exposing (fromFloat, fromInt)
import Svg exposing (Svg)
import Svg.Attributes exposing (fill, fillOpacity, viewBox, x, y)
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
    | Move Bots.Direction
    | MoveTick
    | SpawnTick
    | Spawn Bots.BotType
    | AttackTick


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
    ( { selectionLayer = buildPitch width height, towers = [], cash = 200, health = 10, bots = [ createBot spawnPoint Bots.Default ], state = Running }, Cmd.none )


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
        { position = Points.Point column row, color = defaultColor, opacity = 0.0 } :: buildRow (column - 1) row

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
    "white"


hoverColor : String
hoverColor =
    "grey"


moveBots : List Bot -> List Tower -> Direction -> List Bot
moveBots bots towers direct =
    case bots of
        x :: xs ->
            if collisionEnd x then
                moveBots xs towers direct

            else
                move (collision towers x direct) :: moveBots xs towers direct

        [] ->
            []


collision : List Tower -> Bot -> Direction -> Bot
collision towers bot direct =
    if getY bot.position <= 0 + collidingOffset then
        { bot | direction = Bots.Down }

    else if getY bot.position >= heightInPixels - collidingOffset then
        { bot | direction = Bots.Up }

    else
        collisionTower bot towers direct


collisionEnd : Bot -> Bool
collisionEnd b =
    getX b.position > widthInPixels


collisionTower : Bot -> List Tower -> Direction -> Bot
collisionTower bot towers direct =
    if Lists.any (\p -> collideYBot p.position bot.position) towers then
        { bot | direction = Bots.Down }

    else if Lists.any (\p -> collideYTop p.position bot.position) towers then
        { bot | direction = Bots.Up }

    else if Lists.any (\p -> collideX p.position bot.position) towers then
        if bot.collided == False then
            if getY bot.position > heightInPixels // 2 then
                { bot | direction = direct, collided = True }

            else
                { bot | direction = direct, collided = True }

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
        { bot | health = bot.health - Towers.damage }

    else
        bot


updateTowerHealth : List Bot -> Tower -> Tower
updateTowerHealth bots tower =
    if Lists.any (\p -> isInside tower.position p.position Towers.range) bots then
        { tower | health = tower.health - Towers.damage * 2, attackPoint = addAttackPoint bots tower }

    else
        { tower | attackPoint = Nothing }


addAttackPoint : List Bot -> Tower -> Maybe Point
addAttackPoint bots tower =
    case bots of
        x :: xs ->
            if isInside tower.position x.position Towers.range then
                Just x.position
                {- :: addAttackPoint xs tower -}

            else
                addAttackPoint xs tower

        [] ->
            Nothing


collideX : Point -> Point -> Bool
collideX towerPos botPos =
    toPixels (getX towerPos) - collidingOffset == getX botPos && isBetween (getY towerPos) (getY botPos) collidingOffset


collideYBot : Point -> Point -> Bool
collideYBot towerPos botPos =
    isBetween (getX towerPos) (getX botPos) 0 && toPixels (getY towerPos) + elementSize + collidingOffset == getY botPos


collideYTop : Point -> Point -> Bool
collideYTop towerPos botPos =
    isBetween (getX towerPos) (getX botPos) 0 && toPixels (getY towerPos) - collidingOffset == getY botPos


mark : Point -> Float -> String -> List PitchElement -> List PitchElement
mark p opacity c l =
    case l of
        x :: xs ->
            if x.position == p then
                { x | color = c, opacity = opacity } :: mark p opacity c xs

            else
                x :: mark p opacity c xs

        [] ->
            []


pitchToSvg : List PitchElement -> List Tower -> List Bot -> List (Svg Msg)
pitchToSvg selection tower bot =
    drawBackground :: List.map drawTower tower ++ List.map drawBot bot ++ List.map drawRect selection


drawBackground : Svg Msg
drawBackground =
    Svg.rect
        [ x "0"
        , y "0"
        , Svg.Attributes.width (fromInt widthInPixels)
        , Svg.Attributes.height (fromInt heightInPixels)
        , fill "#ECDCB8"
        ]
        []


drawRect : PitchElement -> Svg Msg
drawRect { position, color, opacity } =
    Svg.rect
        [ Svg.Events.onClick (Click position)
        , Svg.Events.onMouseOut (MouseOut position)
        , Svg.Events.onMouseOver (MouseOver position)
        , x (String.fromInt (getX position * elementSize))
        , y (String.fromInt (getY position * elementSize))
        , Svg.Attributes.width (String.fromInt elementSize)
        , Svg.Attributes.height (String.fromInt elementSize)
        , fill color
        , fillOpacity (fromFloat opacity)
        ]
        []


drawAttackLine : Point -> Maybe Point -> List (Svg Msg)
drawAttackLine towerPos attackPos =
    case attackPos of
        Nothing ->
            []

        Just p ->
            [ Svg.line
                [ Svg.Attributes.x1 (fromInt (toPixels (getX towerPos) + elementSize // 2))
                , Svg.Attributes.y1 (fromInt (toPixels (getY towerPos) + elementSize // 2))
                , Svg.Attributes.x2 (fromInt (getX p))
                , Svg.Attributes.y2 (fromInt (getY p))
                , Svg.Attributes.stroke "#E45040"
                , Svg.Attributes.strokeWidth "3"
                ]
                []
            ]


drawTower : Tower -> Svg Msg
drawTower { position, health, attackPoint } =
    Svg.g
        []
        (Svg.image
            [ x (fromInt (getX position * elementSize))
            , y (fromInt (getY position * elementSize))
            , Svg.Attributes.width (fromInt elementSize)
            , Svg.Attributes.height (fromInt elementSize)
            , Svg.Attributes.xlinkHref "Graphics/tower.png"
            ]
            []
            :: drawAttackLine position attackPoint
            ++ [ Svg.g
                    [Svg.Attributes.transform (Towers.getRotation attackPoint (toPixelPoint position))
                    ]
                    [ Svg.image
                        [ x (fromInt (getX position * elementSize))
                        , y (fromInt (getY position * elementSize))
                        , Svg.Attributes.width (fromInt elementSize)
                        , Svg.Attributes.height (fromInt elementSize)
                        , Svg.Attributes.xlinkHref "Graphics/canon.png"
                        {- , Svg.Attributes.transform (Towers.getRotation attackPoint (toPixelPoint position)) -}
                        ]
                        []
                    , Svg.circle
                        [ Svg.Attributes.cx (fromInt (toPixels (getX position) + elementSize // 2))
                        , Svg.Attributes.cy (fromFloat (toFloat (toPixels (getY position)) + (toFloat elementSize * 0.7)))
                        , Svg.Attributes.r (fromFloat (toFloat elementSize * 0.08))
                        , fill (Towers.updateColor health)
                        ]
                        []
                    ]

               --    , Svg.rect
               --         [ x (fromFloat (toFloat (toPixels (getX position)) + toFloat elementSize * 0.8))
               --         , y (fromInt (toPixels (getY position)))
               --         , Svg.Attributes.width (fromFloat (toFloat elementSize * 0.1))
               --         , Svg.Attributes.height (fromFloat (toFloat elementSize * Towers.healthPointsPercent health * 0.8))
               --         , fill (Towers.updateColor health)
               --         ]
               --         []
               ]
        )


drawBot : Bot -> Svg Msg
drawBot { position, health, botType, direction } =
    Svg.g
        []
        [ Svg.rect
            [ x (fromInt (getX position - round (Bots.size * 0.4)))
            , y (fromInt (getY position - round (Bots.size * 0.6)))
            , Svg.Attributes.width (fromFloat (Bots.size * Bots.healthPointsPercent botType health * 0.8))
            , Svg.Attributes.height (fromFloat (toFloat elementSize * 0.08))
            , fill "green"
            ]
            []
        , Svg.image
            [ x (fromFloat (toFloat (getX position) - Bots.size / 2))
            , y (fromFloat (toFloat (getY position) - Bots.size / 2))
            , Svg.Attributes.transform (Bots.getRoation direction position)
            , Svg.Attributes.xlinkHref (Bots.getImageLink botType)
            , Svg.Attributes.width (fromFloat Bots.size)
            , Svg.Attributes.height (fromFloat Bots.size)
            ]
            []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pos ->
            if model.cash >= Towers.buildCost && Lists.any (\p -> pos == p.position) model.towers == False then
                ( { model | towers = create pos :: model.towers, cash = model.cash - Towers.buildCost }, Cmd.none )

            else if model.cash >= Towers.repairCost && Lists.any (\p -> pos == p.position) model.towers then
                ( { model | towers = Towers.repair pos model.towers, cash = model.cash - Towers.repairCost }, Cmd.none )

            else
                ( model, Cmd.none )

        MouseOver pos ->
            ( { model | selectionLayer = mark pos 0.3 hoverColor model.selectionLayer }, Cmd.none )

        MouseOut pos ->
            ( { model | selectionLayer = mark pos 0.0 defaultColor model.selectionLayer }, Cmd.none )

        MoveTick ->
            ( model, Random.generate Move Bots.directionGenerator )

        Move direct ->
            if Lists.any (\p -> getX p.position > widthInPixels) model.bots then
                ( { model | health = model.health - 1, bots = moveBots model.bots model.towers direct }, Cmd.none )

            else
                ( { model | bots = moveBots model.bots model.towers direct }, Cmd.none )

        SpawnTick ->
            ( model, Random.generate Spawn Bots.botTypeGenerator )

        Spawn botType ->
            ( { model | bots = createBot spawnPoint botType :: model.bots }, Cmd.none )

        AttackTick ->
            if Lists.any (\p -> p.health <= 0) model.bots then
                ( { model | cash = model.cash + Bots.worth, bots = attackBots model.bots model.towers, towers = attackTowers model.towers model.bots }, Cmd.none )

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
            , Html.text (" Build tower: " ++ String.fromInt Towers.buildCost)
            , Html.text (" Repair tower: " ++ String.fromInt Towers.repairCost)
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
            Sub.batch [ Time.every Bots.refreshRate (\_ -> MoveTick), Time.every Bots.spawnRate (\_ -> SpawnTick), Time.every Towers.attackSpeed (\_ -> AttackTick) ]

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
