module Obj exposing (create)

import App exposing (..)
import App.Control exposing (..)
import Appearance exposing (Appearance)
import Body exposing (..)
import Dispatch exposing (..)
import Html exposing (Html)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Model exposing (Inputs)
import Object exposing (..)
import Object.Types exposing (Load(..))
import Orientation
import Vehicle exposing (Driveable)


type alias VehicleAttributes vehicle =
    { drive : Maybe (Driveable vehicle -> Ground -> Inputs -> Moving {} -> Moving {})
    , vehicle : Driveable vehicle
    }

type alias Attributes vehicle =
    { id : String
    , label : String
    , position : Vec3
    , overlay : Html (CtrlMsg Msg)
    , object : ObjectAttributes
    , action : VehicleAttributes vehicle
    }

type alias Model vehicle =
    { motion : Moving {}
    , action : VehicleAttributes vehicle
    , body : Maybe (Moving Body)
    , object : Load ObjectResult
    }


type alias Msg
    = ObjectMsg

create : Attributes d -> ( App, Cmd AppMsg )
create attributes =
    App.create (init attributes)
        { id = always attributes.id
        , label = always attributes.label
        , update = update attributes.action
        , animate = animate
        , bodies = bodies
        , framing = framing
        , focus = focus
        , overlay = always attributes.overlay
        , reposition = reposition
        }


loadBody :
    ( Load ObjectResult, Cmd ObjectMsg )
    -> Model vehicle
    -> ( Model vehicle, Cmd (CtrlMsg Msg) )
loadBody (newObject, newMsg) model =
    let
        mBody =
            case newObject of
                Loading _ ->
                    Nothing
                Ready appear ->
                    Just
                        { anchor = AnchorGround
                        , scale = vec3 1 1 1
                        , position = model.motion.position
                        , orientation = model.motion.orientation
                        , appear = appear
                        , velocity = model.motion.velocity
                       }
    in
        ( { model | object = newObject
                  , body = mBody
          }
        , Cmd.map Self newMsg
        )


init : Attributes vehicle -> ( Model vehicle, Cmd (CtrlMsg Msg) )
init attributes =
    let
        (object, objectCmds) =
            objectInit attributes.object
    in
      loadBody (object, objectCmds)
          { motion =
              { position = attributes.position
              , orientation = Orientation.initial
              , velocity = vec3 0 0 0
              }
          , action = attributes.action
          , body = Nothing
          , object = object
          }


setMotion : Moving {} -> Model vehicle -> Model vehicle
setMotion motion model =
    { model | motion = motion
            , body = Maybe.map (copyMotion motion) model.body
    }


update :
    VehicleAttributes vehicle
    -> CtrlMsg Msg
    -> Model vehicle
    -> ( Model vehicle, Cmd (CtrlMsg Msg) )
update vehicle msg model =
        case msg of
            Self m ->
                loadBody (objectUpdate m model.object) model

            Ctrl (Move dp) ->
                -- ( mapBody (translate dp), Cmd.none)
                ( model, Cmd.none )

            Ctrl (Drive ground inputs) ->
                case vehicle.drive of
                    Just drive ->
                        ( setMotion (drive model.action.vehicle ground inputs model.motion) model
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )

            _ ->
                ( model, Cmd.none )


animate : Ground -> Time -> Model vehicle -> Model vehicle
animate ground dt model =
    let
        aboveGround pos =
            let
                minY = 1.8 + ground.elevation pos
            in
                if V3.getY pos > minY then
                    pos
                else
                    V3.setY minY pos

        motion = model.motion

        newMotion =
            { motion | position = aboveGround motion.position
            }
    in
        setMotion newMotion model


bodies : Model vehicle -> List Body
bodies model_ =
    case model_.body of
        Just body ->
            [ toBody body ]

        Nothing ->
            []


reposition : Maybe AppPosition -> Model vehicle -> Model vehicle
reposition mPos model =
    case mPos of
        Just pos ->
            let
                motion = model.motion
                newMotion =
                    { motion | position = pos.position
                             , orientation = pos.orientation
                    }
            in
                setMotion newMotion model

        Nothing ->
            model


framing : PartyKey -> Model vehicle -> Maybe Framing
framing _ model =
    Maybe.map toFraming model.body


focus : Model vehicle -> Maybe Focus
focus model =
    Maybe.map appToFocus model.body
