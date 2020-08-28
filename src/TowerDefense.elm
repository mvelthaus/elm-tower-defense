-- Author: Mirko Velthaus


module TowerDefense exposing (main)

import Bots exposing (Bot, Direction(..), BotType(..), createBot, move)
import Browser
import Browser.Events
import Html exposing (Html, div, p)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, field, string)
import List
import Lists
import Points exposing (Point(..), elementSize, getX, getY, isBetween, isInside, toPixelPoint, toPixels)
import Random
import String exposing (fromFloat, fromInt)
import Svg exposing (Svg)
import Svg.Attributes exposing (fill, fillOpacity, stroke, strokeWidth, viewBox, x, y)
import Svg.Events exposing (onClick)
import Time exposing (every)
import Towers exposing (Tower, create)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


type Msg
    = Click Point
    | MouseOver Point
    | MouseOut Point
    | Move Bots.Direction
    | MoveTick
    | SpawnTick
    | Spawn Bots.BotType
    | AttackTick
    | TimerTick
    | Action TowerAction
    | Pause
    | Start
    | Other


type TowerAction
    = Build
    | Repair
    | Delete


type State
    = Running Float
    | Lost
    | Paused
    | Prepare Bool


type alias Model =
    { selectionLayer : List PitchElement
    , towers : List Tower
    , cash : Int
    , health : Int
    , bots : List Bots.Bot
    , state : State
    , selection : Point
    , preSelection : Point
    , botsSpawned : Int
    , time : Float
    , score : Int
    }

type alias PitchElement =
    { position : Point
    , opacity : Float
    }

initialModel : ( Model, Cmd Msg )
initialModel =
    ( { selectionLayer = buildPitch width height
      , towers = []
      , cash = 50
      , health = 10
      , bots = []
      , state = Prepare True
      , selection = Point (widthInPixels // 2) (heightInPixels // 2)
      , preSelection = Point 0 0
      , botsSpawned = 0
      , time = 5
      , score = 0
      }
    , Cmd.none
    )



-- PITCH


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
        { position = Point column row, opacity = 0.0 } :: buildRow (column - 1) row

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



-- MOVEMENT

spawnPoint : Point
spawnPoint =
    Point 0 (height * elementSize // 2)

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


collidingOffset : Int
collidingOffset =
    round (toFloat elementSize * 0.4)


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


collideX : Point -> Point -> Bool
collideX towerPos botPos =
    toPixels (getX towerPos) - collidingOffset == getX botPos && isBetween (getY towerPos) (getY botPos) collidingOffset


collideYBot : Point -> Point -> Bool
collideYBot towerPos botPos =
    isBetween (getX towerPos) (getX botPos) 0 && toPixels (getY towerPos) + elementSize + collidingOffset == getY botPos


collideYTop : Point -> Point -> Bool
collideYTop towerPos botPos =
    isBetween (getX towerPos) (getX botPos) 0 && toPixels (getY towerPos) - collidingOffset == getY botPos



-- ATTACKING


attackTowers : List Tower -> List Bot -> List Tower
attackTowers towers bots =
    case bots of
        x :: xs ->
            attackTowers (List.map (\p -> updateTower p x) towers) xs

        [] ->
            destroyTowers towers


attackBots : List Bot -> List Tower -> List Bot
attackBots bots towers =
    case towers of
        x :: xs ->
            attackBots (List.map (\p -> updateBot p x) bots) xs

        [] ->
            bots


killBots : List Bot -> List Bot
killBots bots =
    List.filter (\p -> p.health > 0) bots


destroyTowers : List Tower -> List Tower
destroyTowers towers =
    List.filter (\p -> p.damage > 0) towers


updateTower : Tower -> Bot -> Tower
updateTower t b =
    if isInside t.position b.position Towers.range then
        { t | damage = Towers.calculateDamage t.damage, attackPoint = Just b.position }

    else
        t


updateBot : Bot -> Tower -> Bot
updateBot b t =
    if isInside t.position b.position Towers.range then
        { b | health = b.health - t.damage }

    else
        b



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pos ->
            ( { model | selection = pos }, Cmd.none )

        Action towerAction ->
            updateAction towerAction model

        _ ->
            updateState msg model


updateState : Msg -> Model -> ( Model, Cmd Msg )
updateState msg model =
    case model.state of
        Running _ ->
            if model.health <= 0 then
                ( { model | state = Lost }, Cmd.none )

            else
                updateRunning msg model

        Paused ->
            case msg of
                Pause ->
                    ( { model | state = Running (Bots.spawnRate model.botsSpawned) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Prepare _ ->
            case msg of
                Start ->
                    ( { model | state = Prepare False }, Cmd.none )

                TimerTick ->
                    if model.time <= 0 then
                        ( { model | state = Running (Bots.spawnRate 1), bots = [ createBot spawnPoint Bots.Default ] }, Cmd.none )

                    else
                        ( { model | time = model.time - 1 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateAction : TowerAction -> Model -> ( Model, Cmd Msg )
updateAction towerAction model =
    case towerAction of
        Build ->
            if model.cash >= Towers.buildCost && Lists.any (\p -> model.selection == p.position) model.towers == False then
                ( { model | towers = create model.selection :: model.towers, cash = model.cash - Towers.buildCost }, Cmd.none )

            else
                ( model, Cmd.none )

        Repair ->
            if model.cash >= Towers.repairCost && Lists.any (\p -> model.selection == p.position) model.towers then
                ( { model | towers = Towers.repair model.selection model.towers, cash = model.cash - Towers.repairCost }, Cmd.none )

            else
                ( model, Cmd.none )

        Delete ->
            if Lists.any (\p -> model.selection == p.position) model.towers then
                ( { model | cash = model.cash + 5, towers = Towers.delete model.selection model.towers }, Cmd.none )

            else
                ( model, Cmd.none )


updateRunning : Msg -> Model -> ( Model, Cmd Msg )
updateRunning msg model =
    case msg of
        Pause ->
            ( { model | state = Paused }, Cmd.none )

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
            ( { model | state = Running (Bots.spawnRate model.botsSpawned), bots = createBot spawnPoint botType :: model.bots, botsSpawned = model.botsSpawned + 1 }, Cmd.none )

        AttackTick ->
            ( updateAttack model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateAttack : Model -> Model
updateAttack model =
    if Lists.any (\p -> p.health <= 0) model.bots then
        { model | cash = model.cash + Bots.worth, bots = killBots model.bots, score = model.score + 1 }

    else
        { model | bots = attackBots model.bots model.towers, towers = attackTowers (Towers.resetAttackPoint model.towers) model.bots }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "background-color" "#89A4A6"
        , style "display" "grid"
        , style "grid-template-columns" "1fr 15fr 1fr"
        , style "grid-template-rows" "1fr 25fr 1fr"
        , style "height" "100%"
        , style "color" "white"
        , style "font-family" "\"Trebuchet MS\", Helvetica, sans-serif"
        ]
        [ div [ style "grid-column" "1 / 2", style "grid-row" "2 / 3", style "align-self" "start", style "justify-self" "center" ]
            [ drawImageWithText "cash.png" (String.fromInt model.cash ++ " $") "2em" "70%" Other
            ]
        , div [ style "grid-column" "3 / 4", style "grid-row" "2 / 3", style "align-self" "end", style "justify-self" "center" ]
            [ p [ style "text-align" "center" ] [ Html.text ("Score: " ++ String.fromInt model.score) ]
            , drawImageWithText "health.png" (String.fromInt model.health) "2em" "70%" Other
            ]
        , div [ style "grid-column" "1 / 2", style "grid-row" "2 / 3", style "place-self" "center" ]
            (getTimerText model.state model.time)
        , div [ style "grid-column" "1 / 2", style "grid-row" "2 / 3", style "align-self" "end", style "justify-self" "center" ]
            [ Html.button [ Html.Events.onClick Pause ] [ Html.text (getPauseButtonText model.state) ]
            ]
        , div [ style "grid-column" "2 / 3", style "grid-row" "2 / 3", style "place-self" "center-stretch" ]
            [ Svg.svg
                [ viewBox (String.concat [ "0 0 ", String.fromInt (width * elementSize), " ", String.fromInt (height * elementSize) ])
                ]
                (pitchToSvg model)
            ]
        , div [ style "grid-column" "2 / 3", style "grid-row" "2 / 3", style "place-self" "center", style "color" "black", style "width" "60%", style "text-align" "center" ] (setMessage model)
        , div [ style "grid-column" "3 / 4", style "grid-row" "2 / 3", style "align-self" "start", style "justify-self" "center" ]
            [ drawActionImage "create.png" ("Build (-" ++ String.fromInt Towers.buildCost ++ ")") (Action Build)
            , drawActionImage "repair.png" ("Repair (-" ++ String.fromInt Towers.repairCost ++ ")") (Action Repair)
            , drawActionImage "delete.png" ("Destroy (+" ++ String.fromInt 5 ++ ")") (Action Delete)
            ]
        ]


drawActionImage : String -> String -> Msg -> Html Msg
drawActionImage graphicPath text msg =
    drawImageWithText graphicPath text "0.8em" "auto" msg


drawImageWithText : String -> String -> String -> String -> Msg -> Html Msg
drawImageWithText graphicPath text fontSize imgWidth msg =
    Html.figure [ style "padding" "0px", style "margin" "auto", style "color" "white" ]
        [ Html.img [ Html.Attributes.src ("Graphics/" ++ graphicPath), Html.Events.onClick msg, style "width" imgWidth, style "display" "block", style "margin" "auto" ] []
        , Html.figcaption [ style "text-align" "center", style "font-size" fontSize ] [ Html.text text ]
        ]


pitchToSvg : Model -> List (Svg Msg)
pitchToSvg { selectionLayer, towers, bots, selection } =
    drawBackground :: List.map drawTower towers ++ List.map (\p -> drawSelectionBorder selection p "#eba817") selectionLayer ++ List.map drawBot bots ++ List.map drawRect selectionLayer


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
drawRect { position, opacity } =
    Svg.rect
        [ Svg.Events.onClick (Click position)
        , Svg.Events.onMouseOut (MouseOut position)
        , Svg.Events.onMouseOver (MouseOver position)
        , x (String.fromInt (getX position * elementSize))
        , y (String.fromInt (getY position * elementSize))
        , Svg.Attributes.width (String.fromInt elementSize)
        , Svg.Attributes.height (String.fromInt elementSize)
        , fillOpacity (fromFloat opacity)
        ]
        []


drawTower : Tower -> Svg Msg
drawTower { position, damage, attackPoint } =
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
                    [ Svg.Attributes.transform (Towers.getRotation attackPoint (toPixelPoint position))
                    ]
                    [ Svg.image
                        [ x (fromInt (getX position * elementSize))
                        , y (fromInt (getY position * elementSize))
                        , Svg.Attributes.width (fromInt elementSize)
                        , Svg.Attributes.height (fromInt elementSize)
                        , Svg.Attributes.xlinkHref "Graphics/canon.png"
                        ]
                        []
                    , Svg.circle
                        [ Svg.Attributes.cx (fromInt (toPixels (getX position) + elementSize // 2))
                        , Svg.Attributes.cy (fromFloat (toFloat (toPixels (getY position)) + (toFloat elementSize * 0.7)))
                        , Svg.Attributes.r (fromFloat (toFloat elementSize * 0.08))
                        , fill (Towers.updateColor damage)
                        ]
                        []
                    ]
               ]
        )


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


drawSelectionBorder : Point -> PitchElement -> String -> Svg Msg
drawSelectionBorder p { position } color =
    if p == position then
        Svg.rect
            [ x (fromInt (toPixels (getX position)))
            , y (fromInt (toPixels (getY position)))
            , Svg.Attributes.rx "10"
            , Svg.Attributes.ry "10"
            , Svg.Attributes.width (fromInt elementSize)
            , Svg.Attributes.height (fromInt elementSize)
            , strokeWidth "3"
            , Svg.Attributes.stroke color
            , Svg.Attributes.fillOpacity "0"
            ]
            []

    else
        Svg.rect [] []


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


getPauseButtonText : State -> String
getPauseButtonText state =
    case state of
        Paused ->
            "Continue"

        _ ->
            "Pause"


getTimerText : State -> Float -> List (Html Msg)
getTimerText state time =
    case state of
        Prepare _ ->
            [ p [ style "text-align" "center" ] [ Html.text "Time left to prepare" ]
            , p [ style "text-align" "center", style "font-size" "3em" ] [ Html.text (String.fromFloat time) ]
            ]

        _ ->
            [ p [ style "text-align" "center" ] [ Html.text "Attack started" ] ]


setMessage : Model -> List (Html Msg)
setMessage { state, score } =
    case state of
        Lost ->
            [ p [ style "font-size" "10em" ] [ Html.text "Game Over" ]
            , p [] [ Html.text ("Score: " ++ String.fromInt score) ]
            , p [] [ Html.text "Reload (Press F5) to play again" ]
            ]

        Paused ->
            [ p [ style "font-size" "8em" ] [ Html.text "Pause" ]
            , p [] [ Html.text "You can still optimize your defense. Click \"Start\" or press space when you're ready to continue." ]
            ]

        Prepare help ->
            if help then
                [ Html.h1 [] [ Html.text "How to play" ]
                , p []
                    [ Html.text "This game based on classic tower defense. Attackers spawn in the middle of the left side and try to get to the right. If a attacker reaches the right side you loose a health point. You loose the game when no health points are left." ]
                , p []
                    [ Html.text "You can click anywhere on the pitch and select an action on the upper right or on your keyboard "
                    , Html.b [] [ Html.text "press Q to build, W to repair and E to destroy a tower." ]
                    ]
                , Html.text "Attackers can't move through towers, they have to dodge them. So you can use towers as barricades to slow down the attack."
                , p []
                    [ Html.text "A tower does less damage the more it is used. On the color indicator on the tower you can see you much a tower was used. You can restore the damage by repairing it. When a tower fall down to zero damage it will be destroyed automatically. " ]
                , p [] [ Html.button [ Html.Events.onClick Start ] [ Html.text "Ok. I'm ready to play." ] ]
                ]

            else
                [ Html.text "" ]

        _ ->
            [ Html.text "" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Running spawnrate ->
            Sub.batch [ Browser.Events.onKeyDown keyDecoder, Browser.Events.onAnimationFrame (\_ -> MoveTick), Time.every spawnrate (\_ -> SpawnTick), Time.every Towers.attackSpeed (\_ -> AttackTick) ]

        Prepare False ->
            Sub.batch [ Time.every 1000 (\_ -> TimerTick), Browser.Events.onKeyDown keyDecoder ]

        Prepare True ->
            Sub.batch [ Browser.Events.onKeyDown keyDecoder ]

        Paused ->
            Browser.Events.onKeyDown keyDecoder

        Lost ->
            Sub.none


keyDecoder : Decoder Msg
keyDecoder =
    Json.Decode.map toMsg (field "key" Json.Decode.string)



-- translate input strings to Msg


toMsg : String -> Msg
toMsg s =
    case s of
        "q" ->
            Action Build

        "w" ->
            Action Repair

        "e" ->
            Action Delete

        " " ->
            Pause

        _ ->
            Other
