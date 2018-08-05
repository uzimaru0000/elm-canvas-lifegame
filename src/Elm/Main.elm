module Main exposing (..)

import AnimationFrame exposing (times)
import Canvas exposing (..)
import Color exposing (Color)
import Game exposing (Game)
import Cell exposing (Cell)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Random exposing (Generator)
import Time exposing (Time)
import Tuple


type alias Model =
    { game : Maybe Game
    , frame : Int
    }


type Msg
    = AnimationFrame Time
    | GameGen Game


canvasHeight : Float
canvasHeight =
    320


canvasWidth : Float
canvasWidth =
    320


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    times AnimationFrame


init : ( Model, Cmd Msg )
init =
    ( Model Nothing 0
    , Random.generate GameGen <| Game.generator 64 64
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame time ->
            { model
                | game =
                    if model.frame % 1 == 0 then
                        Maybe.map gameUpdate model.game
                    else
                        model.game
                , frame = model.frame + 1
            }
                ! []

        GameGen newGame ->
            { model | game = Just newGame } ! []


gameUpdate : Game -> Game
gameUpdate game =
    { game
        | cells =
            List.map2 Cell.changeState (Game.adjacencyCellState game) game.cells
    }


clearScreen : Command
clearScreen =
    batch
        [ fillStyle (Color.rgba 255 255 255 0.5)
        , fillRect 0 0 canvasWidth canvasHeight
        ]


view : Model -> Html Msg
view model =
    Html.div []
        [ element
            (round canvasWidth)
            (round canvasHeight)
            [ style [ ( "border", "1px black solid" ) ]
            ]
            [ clearScreen
            , model.game
                |> Maybe.map gameView
                |> Maybe.withDefault []
                |> Canvas.batch
            ]
        ]


gameView : Game -> List Command
gameView { cells, width, height } =
    let
        cellSize =
            ( canvasWidth / ((min width >> toFloat) height)
            , canvasHeight / ((min width >> toFloat) height)
            )
    in
        cells
            |> List.indexedMap (,)
            |> List.map (Tuple.mapFirst <| calcPos width height)
            |> List.map (uncurry <| cellView cellSize)


calcPos : Int -> Int -> Int -> ( Float, Float )
calcPos w h n =
    ( toFloat (n % w), toFloat (n // h) )


cellView : ( Float, Float ) -> ( Float, Float ) -> Cell -> Command
cellView ( w, h ) ( x, y ) cell =
    [ Canvas.fillStyle
        (if cell.state then
            Color.green
         else
            Color.white
        )
    , Canvas.fillRect (x * w) (y * h) w h
    ]
        |> Canvas.batch
