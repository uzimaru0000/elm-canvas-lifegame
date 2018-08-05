module Game exposing (..)

import Cell exposing (Cell)
import Random exposing (Generator)
import List.Extra as List


type alias Game =
    { width : Int
    , height : Int
    , cells : List Cell
    }


type Orientation
    = Up
    | Down
    | Right
    | Left
    | UpLeft
    | UpRight
    | DownLeft
    | DownRight


generator : Int -> Int -> Generator Game
generator w h =
    Cell.generator
        |> Random.list (w * h)
        |> Random.map (Game w h)


displace : Orientation -> Game -> Game
displace ori game =
    case ori of
        Up ->
            let
                cells =
                    List.append
                        (List.drop game.width game.cells)
                        (List.repeat game.width (Cell False))
            in
                { game | cells = cells }

        Down ->
            let
                cells =
                    game.cells
                        |> List.take (game.width * game.height - game.width)
                        |> (++) (List.repeat game.width (Cell False))
            in
                { game | cells = cells }

        Left ->
            let
                cells =
                    game.cells
                        |> List.groupsOf game.width
                        |> List.concatMap (\xs -> List.drop 1 xs ++ [ Cell False ])
            in
                { game | cells = cells }

        Right ->
            let
                cells =
                    game.cells
                        |> List.groupsOf game.width
                        |> List.concatMap (\xs -> [ Cell False ] ++ List.take (game.width - 1) xs)
            in
                { game | cells = cells }

        UpLeft ->
            game
                |> displace Up
                |> displace Left

        UpRight ->
            game
                |> displace Up
                |> displace Right

        DownLeft ->
            game
                |> displace Down
                |> displace Left

        DownRight ->
            game
                |> displace Down
                |> displace Right


adjacencyCellState : Game -> List Int
adjacencyCellState game =
    let
        boolToInt x =
            if x then
                1
            else
                0

        up =
            displace Up game
                |> .cells
                |> List.map (.state >> boolToInt)

        down =
            displace Down game
                |> .cells
                |> List.map (.state >> boolToInt)

        left =
            displace Left game
                |> .cells
                |> List.map (.state >> boolToInt)

        right =
            displace Right game
                |> .cells
                |> List.map (.state >> boolToInt)

        upLeft =
            displace UpLeft game
                |> .cells
                |> List.map (.state >> boolToInt)

        upRight =
            displace UpRight game
                |> .cells
                |> List.map (.state >> boolToInt)

        downLeft =
            displace DownLeft game
                |> .cells
                |> List.map (.state >> boolToInt)

        downRight =
            displace DownRight game
                |> .cells
                |> List.map (.state >> boolToInt)
    in
        List.map4 (\a b c d -> a + b + c + d) up down left right
            |> List.map5 (\a b c d e -> a + b + c + d + e) upLeft upRight downLeft downRight
