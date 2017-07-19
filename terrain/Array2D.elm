module Array2D exposing (..)

import Array
import List.Extra exposing (splitAt)


type Array2D a
    = Array2D Int (Array.Array a)


fromArray : Array.Array a -> Array2D a
fromArray arr =
    let
        s =
            round <| sqrt <| toFloat <| Array.length arr
    in
        Array2D s arr


fromLists : List (List a) -> Array2D a
fromLists =
    fromArray << Array.fromList << List.concat


toLists : Array2D a -> List (List a)
toLists (Array2D s arr) =
    splitEvery s (Array.toList arr)


getXY : Int -> Int -> a -> Array2D a -> a
getXY x y def (Array2D s arr) =
    Maybe.withDefault def (Array.get (x + y * s) arr)


setXY : Int -> Int -> a -> Array2D a -> Array2D a
setXY x y val (Array2D s arr) =
    Array2D s (Array.set (x + y * s) val arr)


mapXY : Int -> Int -> (a -> a) -> Array2D a -> Array2D a
mapXY x y f input =
    let
        (Array2D s arr) = input
    in
        Array.get (x + y * s) arr
        |> Maybe.map (\val -> setXY x y (f val) input)
        |> Maybe.withDefault input


map : (a -> b) -> Array2D a -> Array2D b
map f (Array2D s arr) =
    Array2D s (Array.map f arr)


splitEvery : Int -> List a -> List (List a)
splitEvery size xs =
    case xs of
        [] ->
            []

        _ ->
            let
                ( p, q ) =
                    splitAt size xs
            in
                p :: splitEvery size q
