module Cell exposing (Cell, changeState, generator)

import Random exposing (Generator)


type alias Cell =
    { state : Bool
    }


generator : Generator Cell
generator =
    Random.int 0 1
        |> Random.map ((==) 1)
        |> Random.map Cell


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
