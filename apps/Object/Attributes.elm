module Object.Attributes exposing
    ( VehicleAttributes
    , Action(..)
    , Attributes
    , Update
    , id
    , label
    , position
    , overlay
    , object
    , action
    )

import App exposing (..)
import App.Control exposing (CtrlMsg)
import Body exposing (..)
import Html exposing (Html)
import Math.Vector3 as V3 exposing (Vec3)
import Model exposing (Inputs)
import Object exposing (ObjectAttributes)
import Vehicle exposing (Driveable)

type alias VehicleAttributes vehicle =
    { drive : Maybe (Driveable vehicle -> Ground -> Inputs -> Moving {} -> Moving {})
    , vehicle : Driveable vehicle
    }

type Action vehicle
    = Statue
    | Vehicle (VehicleAttributes vehicle)

type alias Attributes vehicle msg =
    { id : String
    , label : String
    , position : Vec3
    , overlay : Html (CtrlMsg msg)
    , object : ObjectAttributes
    , action : Action vehicle
    }

type alias Update vehicle msg = 
    Attributes vehicle msg -> Attributes vehicle msg

id : String -> Update vehicle msg
id s attr = { attr | id = s }

label : String -> Update vehicle msg
label l attr = { attr | label = l }

position : Vec3 -> Update vehicle msg
position pos attr = { attr | position = pos }

overlay : Html (CtrlMsg msg) -> Update vehicle msg
overlay o attr = { attr | overlay = o }

object : ObjectAttributes -> Update vehicle msg
object o attr = { attr | object = o }

action : Action vehicle -> Update vehicle msg
action a attr = { attr | action = a }
