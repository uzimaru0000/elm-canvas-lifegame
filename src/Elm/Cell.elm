module Cell exposing (..)

import Random exposing (Generator)


type alias Cell =
    { state : Bool
    }


generator : Generator Cell
generator =
    Random.map Cell Random.bool


changeState : Int -> Cell -> Cell
changeState n { state } =
    case ( n, state ) of
        ( 3, False ) ->
            Cell True

        ( 2, True ) ->
            Cell True

        ( 3, True ) ->
            Cell True

        _ ->
            Cell False
