-- Author: Mirko Velthaus


module Snake exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, field, string)
import Lists
import NELists
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (..)


type Point
    = Point Int Int


speed : Float
speed =
    200


type Direction
    = Up
    | Right
    | Down
    | Left


type State
    = Running Direction
    | Lost
    | Paused


type alias Snake =
    NELists.NEList Point


type alias Model =
    { snake : Snake
    , state : State
    , food : Point
    }


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { snake = startSnake 5 (Point elementSize elementSize), state = Running Right, food = Point (pitchSize // 2) (pitchSize // 2) }, Random.generate Ate pointGenerator )


type Msg
    = Dir Direction
    | Click Int
    | Tick
    | Pause
    | Ate Point
    | Other



-- Size and moving distance of the points


elementSize : Int
elementSize =
    20


pitchSize : Int
pitchSize =
    600



-- generate the initial snake


startSnake : Int -> Point -> Snake
startSnake n p =
    moveSnake Right (NELists.repeat n p)



-- move a point in a specific direction


movePoint : Direction -> Point -> Point
movePoint d (Point x y) =
    case d of
        Up ->
            Point x (y - elementSize)

        Right ->
            Point (x + elementSize) y

        Down ->
            Point x (y + elementSize)

        Left ->
            Point (x - elementSize) y


moveSnake : Direction -> Snake -> Snake
moveSnake d (NELists.Head x xs) =
    NELists.Head (movePoint d x) (NELists.removeLast (NELists.Head x xs))



-- check the snake for collison with bounds, food or own elements


outOfBounds : Snake -> Bool
outOfBounds (NELists.Head (Point x y) _) =
    x <= 0 - elementSize || x >= pitchSize || y <= 0 - elementSize || y >= pitchSize


collision : Snake -> Bool
collision (NELists.Head x xs) =
    Lists.any (\p -> x == p) xs


foodCollision : Snake -> Point -> Bool
foodCollision (NELists.Head x _) p =
    x == p



-- add an element at the end of the snake


snoc : Snake -> Point -> Snake
snoc s p =
    NELists.snoc s p



-- transform the snake to svg


snakeToSvg : Snake -> List (Svg Msg)
snakeToSvg (NELists.Head x xs) =
    drawRect x "red" :: tailToSvg xs


tailToSvg : List Point -> List (Svg Msg)
tailToSvg l =
    Lists.indexedMap colorize l


colorize : Int -> Point -> Svg Msg
colorize n p =
    drawRect p (segmentColor n)



-- draw a single snake element as svg


drawRect : Point -> String -> Svg Msg
drawRect (Point xPoint yPoint) color =
    rect
        [ x (String.fromInt xPoint)
        , y (String.fromInt yPoint)
        , Svg.Attributes.width (String.fromInt elementSize)
        , Svg.Attributes.height (String.fromInt elementSize)
        , fill color
        ]
        []


drawPitch : Svg Msg
drawPitch =
    rect
        [ x "0"
        , y "0"
        , Svg.Attributes.width (String.fromInt pitchSize)
        , Svg.Attributes.height (String.fromInt pitchSize)
        , fill "lightgrey"
        ]
        []



-- set the message the user sees when running or stopped the application


setMessage : State -> Html Msg
setMessage state =
    case state of
        Running _ ->
            Html.text ""

        Lost ->
            Html.text "Lost. Reload to play again."

        Paused ->
            Html.text "Paused. Press space to continue."



-- generate a string for the colors


segmentColor : Int -> String
segmentColor n =
    hsl (30 * n) 100 40


hsl : Int -> Int -> Int -> String
hsl h s l =
    "hsl(" ++ String.fromInt h ++ " ," ++ String.fromInt s ++ "% ," ++ String.fromInt l ++ "%)"


keyDecoder : Decoder Msg
keyDecoder =
    Json.Decode.map toMsg (field "key" Json.Decode.string)



-- translate input strings to Msg


toMsg : String -> Msg
toMsg s =
    case s of
        "ArrowUp" ->
            Dir Up

        "ArrowDown" ->
            Dir Down

        "ArrowRight" ->
            Dir Right

        "ArrowLeft" ->
            Dir Left

        " " ->
            Pause

        _ ->
            Other


pointGenerator : Random.Generator Point
pointGenerator =
    Random.uniform (Point 0 0) (generatePitchPoints 0 0)



-- generates a list of possible psotions for the food


generatePitchPoints : Int -> Int -> List Point
generatePitchPoints n m =
    if n < pitchSize then
        if m < pitchSize then
            Point n m :: generatePitchPoints (n + elementSize) m

        else
            []

    else
        generatePitchPoints 0 (m + elementSize)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click _ ->
            ( { model | state = Running Down }, Cmd.none )
        Dir d ->
            ( { model | state = Running d }, Cmd.none )

        Tick ->
            case model.state of
                Running d ->
                    if outOfBounds model.snake || collision model.snake then
                        ( { model | state = Lost }, Cmd.none )

                    else if foodCollision model.snake model.food then
                        ( { model | snake = moveSnake d model.snake }, Random.generate Ate pointGenerator )

                    else
                        ( { model | snake = moveSnake d model.snake }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Pause ->
            if model.state == Paused then
                ( { model | state = Running Right }, Cmd.none )

            else
                ( { model | state = Paused }, Cmd.none )

        Ate p ->
            ( { model | snake = snoc model.snake (NELists.last model.snake), food = p }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ onClick (Click 1), Html.Attributes.style "width" "35%", Html.Attributes.style "margin-right" "auto", Html.Attributes.style "margin-left" "auto", Html.Attributes.style "margin-top" "1%" ]
        [ div [ Html.Attributes.style "height" "250px" ]
            [ Html.button [ onClick (Click 5) ] [ Html.text "Up" ]
              -- , Html.button [ onClick (Dir Left) ] [ Html.text "Left" ]
              -- , Html.button [ onClick (Dir Right) ] [ Html.text "Right" ]
              -- , Html.button [ onClick (Dir Down) ] [ Html.text "Down" ]
              --, Html.button [ onClick Extend ] [ Html.text "Extend" ]
            , Html.text ("Score: " ++ String.fromInt (Lists.length (NELists.toList model.snake)) ++ ", control snake with arrow keys")
            , div []
                [ svg
                    [ viewBox "0 0 600 600"
                    ]
                    (drawPitch :: drawRect model.food "Green" :: snakeToSvg model.snake)
                ]
            ]
        , setMessage model.state
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Running _ ->
            Sub.batch [ Browser.Events.onKeyDown keyDecoder, Time.every speed (\_ -> Tick) ]

        Paused ->
            Browser.Events.onKeyDown keyDecoder

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
