module TowerDefense exposing (main)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Browser exposing (element)


type Point
    = Point Int Int

getX : Point -> Int
getX (Point x _) =
    x

getY : Point -> Int
getY (Point _ y) =
    y

type alias PitchElement =
    {position : Point
    , color : String
    , hasTower : Bool
    }


type Msg
    = Click Point
    -- | MouseOver Point
    -- | MouseOut Point


type State
    = Running
    | Lost
    | Paused


type alias Playground =
    List PitchElement


type alias Bots =
    List Point


type alias Model =
    { playground : Playground
    , bots : Bots
    , state : State
    }


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { playground = buildPitch width height, bots = List.repeat 1 (Point 0 0), state = Running }, Cmd.none )


buildPitch : Int -> Int -> Playground
buildPitch w h =
    let
        buildRows row =
            if row > 0 then
                buildRow w row ++ buildRows (row - 1)

            else
                []
    in
    buildRows h


buildRow : Int -> Int -> List PitchElement
buildRow column row =
    if column > 0 then
        {position = Point column row, color = defaultColor, hasTower = False} :: buildRow (column - 1) row

    else
        []


elementSize : Int
elementSize =
    20


width : Int
width =
    20


height : Int
height =
    15


defaultColor : String
defaultColor =
    "lightgrey"


segmentColor : Int -> String
segmentColor n =
    hsl (30 * n) 100 40


hsl : Int -> Int -> Int -> String
hsl h s l =
    "hsl(" ++ String.fromInt h ++ " ," ++ String.fromInt s ++ "% ," ++ String.fromInt l ++ "%)"



-- playgroundToSvg : Playground -> List (Svg Msg)
-- playgroundToSvg playground =
--     case playground of
--         NELists.Head x xs ->
--             drawRect x "white" :: playgroundToSvg xs
--         NELists.Head x [] ->
--             []


pitchToSvg : Playground -> List (Svg Msg)
pitchToSvg l =
    List.map drawRect l


drawRect : PitchElement -> Svg Msg
drawRect {position, color} =
    rect
        [ Svg.Events.onClick (Click position)
        -- , Svg.Events.onMouseOut (MouseOut position)
        -- , Svg.Events.onMouseOver (MouseOver position)
        , Svg.Attributes.style "border: 1px solid white"
        , x (String.fromInt (getX position * elementSize))
        , y (String.fromInt (getY position * elementSize))
        , Svg.Attributes.width (String.fromInt elementSize)
        , Svg.Attributes.height (String.fromInt elementSize)
        , fill color
        ]
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pos ->
            ( { model | playground = mark pos "red" model.playground }, Cmd.none )

        -- MouseOver pos ->
        --     ( { model | playground = mark pos "grey" model.playground }, Cmd.none )

        -- MouseOut pos ->
        --     ( { model | playground = mark pos defaultColor model.playground }, Cmd.none )



mark : Point -> String -> List PitchElement -> List PitchElement
mark p c l =
    case l of
        element :: xs ->
            if element.position == p then
                {element | color = c} :: mark p c xs

            else
                element :: mark p c xs

        [] ->
            []


view : Model -> Html Msg
view model =
    div [ {- onClick (Click 1), -} Html.Attributes.style "width" "35%", Html.Attributes.style "margin-right" "auto", Html.Attributes.style "margin-left" "auto", Html.Attributes.style "margin-top" "1%" ]
        [ div [ Html.Attributes.style "height" "250px" ]
            [ {- Html.button [ onClick (Click 5) ] [ Html.text "Up" ] -}
              -- , Html.button [ onClick (Dir Left) ] [ Html.text "Left" ]
              -- , Html.button [ onClick (Dir Right) ] [ Html.text "Right" ]
              -- , Html.button [ onClick (Dir Down) ] [ Html.text "Down" ]
              --, Html.button [ onClick Extend ] [ Html.text "Extend" ]
              div []
                [ svg
                    [ viewBox (String.concat [ "0 0 ", String.fromInt (width * elementSize), " ", String.fromInt (height * elementSize) ])
                    ]
                    (pitchToSvg model.playground)
                ]
            ]

        -- , setMessage model.state
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
