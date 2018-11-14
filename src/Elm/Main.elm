module Main exposing (Model, Msg(..), calcPos, canvasHeight, canvasWidth, cellView, clearScreen, gameUpdate, gameView, init, main, subscriptions, update, view)

import Browser exposing (element)
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (..)
import CanvasColor exposing (Color)
import Cell exposing (Cell)
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Random exposing (Generator)
import Time exposing (every)


type alias Model =
    { game : Maybe Game
    , frame : Int
    }


type Msg
    = AnimationFrame Time.Posix
    | GameGen Game


canvasHeight : Float
canvasHeight =
    640


canvasWidth : Float
canvasWidth =
    640


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrame AnimationFrame


init : ( Model, Cmd Msg )
init =
    ( Model Nothing 0
    , Random.generate GameGen <| Game.generator 60 60
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame time ->
            ( { model
                | game = Maybe.map gameUpdate model.game
              }
            , Cmd.none
            )

        GameGen newGame ->
            ( { model | game = Just newGame }, Cmd.none )


gameUpdate : Game -> Game
gameUpdate game =
    { game
        | cells =
            List.map2 Cell.changeState (Game.adjacencyCellState game) game.cells
    }


clearScreen : Commands
clearScreen =
    Canvas.empty
        |> fillStyle (CanvasColor.rgba 255 255 255 0.1)
        |> fillRect 0 0 canvasWidth canvasHeight


view : Model -> Html Msg
view model =
    Html.div []
        [ Canvas.element
            (round canvasWidth)
            (round canvasHeight)
            [ style "border" "1px black solid"
            ]
            (clearScreen
                |> gameView model.game
            )
        ]


gameView : Maybe Game -> Commands -> Commands
gameView game cmds =
    case game of
        Just { cells, width, height } ->
            let
                cellSize =
                    ( canvasWidth / (min width >> toFloat) height
                    , canvasHeight / (min width >> toFloat) height
                    )
            in
            cells
                |> List.indexedMap Tuple.pair
                |> List.map (Tuple.mapFirst <| calcPos width height)
                |> List.foldl (\( pos, cell ) acc -> cellView acc cellSize pos cell) cmds

        Nothing ->
            Canvas.empty


calcPos : Int -> Int -> Int -> ( Float, Float )
calcPos w h n =
    ( toFloat (modBy w n)
    , toFloat (n // h)
    )


cellView : Commands -> ( Float, Float ) -> ( Float, Float ) -> Cell -> Commands
cellView cmds ( w, h ) ( x, y ) cell =
    cmds
        |> Canvas.fillStyle
            (if cell.state then
                CanvasColor.green

             else
                CanvasColor.white
            )
        |> Canvas.fillRect (x * w) (y * h) w h
