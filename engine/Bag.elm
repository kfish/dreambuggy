module Bag exposing (Bag, Key, size, empty, insert, replace, get, keys, items, map)

import Dict exposing (Dict)


type alias Key =
    Int


type Bag v
    = Bag Int (Dict Key v)


size : Bag v -> Int
size (Bag n _) =
    n


empty : Bag v
empty =
    Bag 0 (Dict.empty)


insert : v -> Bag v -> ( Key, Bag v )
insert v (Bag n dict) =
    ( n, Bag (n + 1) (Dict.insert n v dict) )



-- If you do Bag.replace with a key that you didn't get from an insert, then you might lose this
-- item on a subsequent insert


replace : Key -> v -> Bag v -> Bag v
replace key v (Bag n dict) =
    Bag n (Dict.insert key v dict)


get : Key -> Bag v -> Maybe v
get key (Bag _ dict) =
    Dict.get key dict


keys : Bag v -> List Key
keys (Bag _ dict) =
    Dict.keys dict


items : Bag v -> List v
items (Bag _ dict) =
    Dict.values dict


map : (a -> b) -> Bag a -> Bag b
map f (Bag n dict) =
    Bag n (Dict.map (\_ -> f) dict)
