module TestMain exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import TestExtend exposing (..)


-- import data module

import Game exposing (Game, Orientation)
import Cell exposing (Cell)


game1 : Game
game1 =
    { cells =
        [ Cell False
        , Cell True
        , Cell False
        , Cell True
        , Cell False
        , Cell True
        , Cell False
        , Cell True
        , Cell False
        ]
    , width = 3
    , height = 3
    }


game2 : Game
game2 =
    { cells =
        [ Cell False
        , Cell True
        , Cell False
        , Cell True
        , Cell False
        , Cell True
        , Cell False
        , Cell True
        , Cell False
        , Cell True
        , Cell False
        , Cell True
        ]
    , width = 3
    , height = 4
    }


game3 : Game
game3 =
    { cells =
        [ Cell False
        , Cell True
        , Cell False
        , Cell True
        , Cell False
        , Cell True
        , Cell False
        , Cell True
        , Cell False
        , Cell True
        , Cell False
        , Cell True
        ]
    , width = 4
    , height = 3
    }


all : Test
all =
    describe "allTest"
        [ upCase
        , downCase
        , leftCase
        , rightCase
        ]


displaceTest : String -> Orientation -> ( Game, List Cell ) -> Test
displaceTest str ori ( data, ans ) =
    str
        => (Game.displace ori data
                |> .cells
           )
        === ans


upCase : Test
upCase =
    describe "Case Up"
        [ displaceTest "w == h"
            Game.Up
            ( game1
            , [ Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell False
              , Cell False
              , Cell False
              ]
            )
        , displaceTest "w < h"
            Game.Up
            ( game2
            , [ Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell False
              , Cell False
              ]
            )
        , displaceTest "w > h"
            Game.Up
            ( game3
            , [ Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell False
              , Cell False
              , Cell False
              ]
            )
        ]


downCase : Test
downCase =
    describe "Case Down"
        [ displaceTest "w == h"
            Game.Down
            ( game1
            , [ Cell False
              , Cell False
              , Cell False
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              ]
            )
        , displaceTest "w < h"
            Game.Down
            ( game2
            , [ Cell False
              , Cell False
              , Cell False
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              ]
            )
        , displaceTest "w > h"
            Game.Down
            ( game3
            , [ Cell False
              , Cell False
              , Cell False
              , Cell False
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              ]
            )
        ]


leftCase : Test
leftCase =
    describe "Case Left"
        [ displaceTest "w == h"
            Game.Left
            ( game1
            , [ Cell True
              , Cell False
              , Cell False
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell False
              ]
            )
        , displaceTest "w < h"
            Game.Left
            ( game2
            , [ Cell True
              , Cell False
              , Cell False
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell False
              , Cell False
              , Cell True
              , Cell False
              ]
            )
        , displaceTest "w > h"
            Game.Left
            ( game3
            , [ Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              ]
            )
        ]


rightCase : Test
rightCase =
    describe "Case Right"
        [ displaceTest "w == h"
            Game.Right
            ( game1
            , [ Cell False
              , Cell False
              , Cell False
              , Cell False
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              ]
            )
        , displaceTest "w < h"
            Game.Right
            ( game2
            , [ Cell False
              , Cell False
              , Cell False
              , Cell False
              , Cell False
              , Cell True
              , Cell False
              , Cell True
              , Cell False
              , Cell False
              , Cell False
              , Cell True
              ]
            )
        , displaceTest "w > h"
            Game.Right
            ( game3
            , [ Cell False
              , Cell False
              , Cell False
              , Cell False
              , Cell False
              , Cell False
              , Cell True
              , Cell False
              , Cell False
              , Cell False
              , Cell True
              , Cell False
              ]
            )
        ]
