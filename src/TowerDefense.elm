module TowerDefense exposing (main)

import Browser
import Html exposing (Html, div, button)
import Html.Attributes exposing (style)
import NELists exposing (NEList)
import Lists
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)

type Point
    = Point Int Int

type Msg
    = Click Int

type State
    = Running
    | Lost
    | Paused

type alias Playground =
    NELists.NEList Point

type alias Bots =
    List Point

type alias Model =
    {playground : Playground
    , bots : Bots
    , state : State}

initialModel : ( Model, Cmd Msg)
initialModel = 
    ({playground = NELists.repeat 10 Point 0 0, bots = List.repeat 1 Point 0 0, state = Running})

playgroundToSvg : Playground -> Svg Msg
playgroundToSvg (NELists.Head x xs) =
    case x of
       Point 0 0 ->
        rect [][]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click _ ->
            (model, Cmd.none)


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
                    (playgroundToSvg model.playground)
                ]
            ]
        -- , setMessage model.state
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Running ->
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