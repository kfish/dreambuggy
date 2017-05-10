module Update exposing (update)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Time exposing (Time)

import Bag
import Dispatch exposing (..)
import Model exposing (Model, Msg)
import Orientation
import Math.Quaternion as Qn -- Don't expose this here
import Ports

import Gamepad
import GamepadInputs

import Control exposing (WorldMsg)
import Body exposing (Camera)
import App exposing (Focus)
import Ground exposing (Ground)
import Vehicles.DreamBird as DreamBird
import Vehicles.DreamBuggy as DreamBuggy
import Vehicles.LookAt as LookAt
import Vehicles.DreamDebug as DreamDebug

{-| Take a Msg and a Model and return an updated Model
-}
update : (WorldMsg worldMsg -> worldModel -> (worldModel, Cmd (WorldMsg worldMsg)))
    -> (Maybe Bag.Key -> worldModel -> String)
    -> (worldModel -> Int)
    -> (worldModel -> Maybe Ground)
    -> (Time -> worldModel -> worldModel)
    -> (Maybe Bag.Key -> worldModel -> Maybe Camera)
    -> (Bag.Key -> worldModel -> Maybe Focus)
    -> Model.Msg (WorldMsg worldMsg) -> Model worldModel
    -> (Model worldModel, Cmd (Msg (WorldMsg worldMsg)))
update worldUpdate worldLabel worldKeyLimit worldTerrain worldAnimate worldCamera worldFocus msg model =
    case msg of
        Model.WorldMessage worldMsg ->
            let (worldModel, worldCmdMsg) = worldUpdate worldMsg model.worldModel in
            ( { model | worldModel = worldModel }, Cmd.map Model.WorldMessage worldCmdMsg )
        Model.KeyChange keyfunc ->
            let keys = keyfunc model.keys in
            ( { model | keys = keys
                      , inputs = keysToInputs keys model.inputs }
            , Cmd.none )
        Model.Resize windowSize ->
            ( { model | maybeWindowSize = Just windowSize }, Cmd.none )
        Model.MouseMove movement ->
            ( { model | inputs = mouseToInputs movement model.inputs }, Cmd.none )
        Model.GamepadUpdate gps0 ->
            ( updateGamepads gps0 model, Cmd.none )
        Model.LockRequest wantToBeLocked ->
            ( { model | wantToBeLocked = wantToBeLocked }
            , if model.wantToBeLocked == model.isLocked then
                Cmd.none
              else if model.wantToBeLocked then
                Ports.requestPointerLock ()
              else
                Ports.exitPointerLock ()
            ) 
        Model.LockUpdate isLocked ->
            ( { model | isLocked = isLocked }, Cmd.none )
        Model.Animate dt ->
            let (model_, newCmdMsg) = case worldTerrain model.worldModel of
                Nothing -> (model, Cmd.none)
                Just terrain ->
                    let inputs1 = timeToInputs dt model.inputs
                        inputs2 = timeToInputs dt model.inputs2
                        keyLimit = worldKeyLimit model.worldModel

                        -- Animate
                        wm = worldAnimate inputs1.dt model.worldModel

                        -- Camera
                        label1 = worldLabel (model.player1.rideKey) model.worldModel
                        camera1 = worldCamera (model.player1.rideKey) model.worldModel
                        (wm1, wm1Msg) = case model.player1.rideKey of
                            Just key -> worldUpdate (Forward key (Control.Drive inputs1)) wm
                            Nothing  -> (wm, Cmd.none)

                        label2 = worldLabel (model.player2.rideKey) model.worldModel
                        camera2 = worldCamera (model.player2.rideKey) model.worldModel
                        (wm2, wm2Msg) = case model.player2.rideKey of
                            Just key -> worldUpdate (Forward key (Control.Drive inputs2)) wm1
                            Nothing  -> (wm1, Cmd.none)

                        -- Focus
                        (wmF, wmFMsg, focPos) = let key = model.player1.focusKey in
                            case worldFocus key model.worldModel of
                                Just focus ->
                                    let dp = inputsToMove inputs1 model.player1
                                        (wmF, wmFMsg) =
                                            worldUpdate (Forward key (Control.Move dp)) wm2
                                    in (wmF, wmFMsg, Just focus.pos)
                                _ -> (wm2, Cmd.none, Nothing)

                        newModel =
                            { model | globalTime = model.globalTime + dt
                                    , player1 = step terrain keyLimit inputs1 label1 camera1 focPos model.player1
                                    , player2 = step terrain keyLimit inputs2 label2 camera2 Nothing model.player2
                                    , inputs = clearStationaryInputs inputs1
                                    , worldModel = wmF
                            }
                    in -- Cmd.batch ]wm1Msg, wm2Msg, wmFMsg]
                        (newModel, Cmd.map Model.WorldMessage wmFMsg)
            in ( model_
               , Cmd.batch
                   [ Gamepad.gamepads Model.GamepadUpdate
                   , newCmdMsg
                   ]
               )

inputsToMove : Model.Inputs -> Model.Player -> Vec3
inputsToMove inputs player =
    let dp = vec3 -inputs.cx 0 inputs.cy
    in Orientation.rotateBodyV player.motion.orientation dp

timeToInputs : Time -> Model.Inputs -> Model.Inputs
timeToInputs dt inputs0 = { inputs0 | dt = dt }

keysToInputs : Model.Keys -> Model.Inputs -> Model.Inputs
keysToInputs keys inputs0 =
    let minusPlus a b = if a && not b then -1 else if b && not a then 1 else 0
    in
        { inputs0 | x = minusPlus keys.left keys.right
                  , y = minusPlus keys.down keys.up
                  , button_X = keys.space
        }

mouseToInputs : Model.MouseMovement -> Model.Inputs -> Model.Inputs
mouseToInputs (mx,my) inputs0 = { inputs0 | mx = toFloat mx / 500, my = toFloat my / 500 }

clearStationaryInputs : Model.Inputs -> Model.Inputs
clearStationaryInputs inputs0 = { inputs0 | mx = 0, my = 0 }

gamepadToInputs : Gamepad.Gamepad -> Model.Inputs -> Model.Inputs
gamepadToInputs gamepad0 inputs0 =
    let gamepad = GamepadInputs.toStandardGamepad gamepad0
        {x,y,mx,my,cx,cy} = GamepadInputs.gamepadToArrows gamepad
        bs = GamepadInputs.gamepadToButtons gamepad
        risingEdge old new = new && (not old)
    in  { inputs0 | reset = bs.bStart
                  , changeVR = risingEdge inputs0.changeVR bs.bB
                  , changeCamera = risingEdge inputs0.changeCamera bs.bRightBumper
                  , x = x, y = y
                  , mx=mx, my=my
                  , cx=cx, cy=cy
                  , button_X = risingEdge inputs0.button_X bs.bX }

updateGamepads : List Gamepad.Gamepad -> Model worldModel -> Model worldModel
updateGamepads gps0 model =
    let (gps, is) = GamepadInputs.persistentGamepads model.gamepadIds gps0 in
    case gps of
      [] -> model
      [Just gp] ->
        { model | numPlayers = 1
                , inputs = gamepadToInputs gp model.inputs
                , gamepadIds = is
        }
      (Just gp :: Nothing :: _) ->
        { model | numPlayers = 1
                , inputs = gamepadToInputs gp model.inputs
                , gamepadIds = is
        }
      (Nothing :: Just gp2 :: _) ->
        { model | numPlayers = 2
                , inputs2 = gamepadToInputs gp2 model.inputs2
                , gamepadIds = is
        }
      (Just gp :: Just gp2 :: _) ->
        { model | numPlayers = 2
                , inputs = gamepadToInputs gp model.inputs
                , inputs2 = gamepadToInputs gp2 model.inputs2
                , gamepadIds = is
        }
      _ -> model

aboveGround : Model.EyeLevel -> Vec3 -> Vec3
aboveGround eyeLevel pos =
    let
        p = toRecord pos
        e = eyeLevel pos
    in
        if p.y < e then vec3 p.x e p.z else pos

step : Ground -> Int -> Model.Inputs -> String -> Maybe Camera -> Maybe Vec3 -> Model.Player -> Model.Player
step terrain keyLimit inputs label camera focPos player0 = if inputs.reset then Model.defaultPlayer else
        let 
            eyeLevel pos = Model.eyeLevel + terrain.elevation pos

            move player = case camera of
                Just c  -> { player | motion = { position = c.position,
                                                 velocity = player.motion.velocity,
                                                 orientation = Qn.fromVec3 c.orientation
                                               }
                           }
                Nothing -> player
{-
            move player = case player.vehicle of
                Just v  -> mapMotion (v.move focPos eyeLevel inputs) player
                Nothing -> player
-}

            relabel player = { player | rideLabel = label }

            keepWithinbounds motion = { motion | position = terrain.bounds motion.position }

            checkCamera player = { player |
                cameraInside = if inputs.changeCamera then
                                   not player.cameraInside
                               else
                                   player.cameraInside,
                cameraVR = if inputs.changeVR then
                                   not player.cameraVR
                               else
                                   player.cameraVR }

            moveCamera player =
                if player.cameraInside then
                    -- let behind = player.pos `sub` (V3.scale 2.5 (Model.direction player.motion)) `sub` (vec3 0 0.5 0)
                    let inside = add player.motion.position
                                     (Orientation.rotateBodyV player.motion.orientation (vec3 0 0 3.0)) -- wedge
                                     -- Inside Jeep driver's seat
                                     -- `add` Qn.vrotate player.orientQn (vec3 0.38 0.5 -2.3)
                    in
                        { player | cameraPos = inside -- aboveGround eyeLevel behind
                                 , cameraUp = Model.cameraUp player }
                else
                    let behind = sub player.motion.position (V3.scale 7 (Model.direction player.motion))
                        p = toRecord player.motion.position
                        yMax0 v = let vr = V3.toRecord v in vec3 vr.x (min (-0.3) vr.y) vr.z
                        newCameraPos =
                            if p.y < Model.eyeLevel then
                                yMax0 (add (vec3 0 2 0) behind)
                            else if p.y < Model.eyeLevel+1 then
                                behind
                            else
                                -- add (vec3 0 2 0) behind
                                add (vec3 0 -2 0) behind
                        cameraPos = aboveGround eyeLevel
                            -- (V3.scale 0.5 newCameraPos `add` V3.scale 0.5 player.cameraPos) -- smooth
                            newCameraPos
                        newCameraUp = Model.cameraUp player

                    in  { player | cameraPos = terrain.bounds cameraPos
                                 , cameraUp =
                            -- V3.scale 0.1 newCameraUp `add` V3.scale 0.9 player.cameraUp }
                               newCameraUp }
        in
            player0
                |> mapMotion (gravity eyeLevel inputs.dt)
                |> selectVehicle keyLimit inputs
                |> relabel
                |> move
                |> mapMotion keepWithinbounds
                |> checkCamera
                |> moveCamera

selectVehicle : Int -> Model.Inputs -> Model.Player -> Model.Player
selectVehicle keyLimit inputs player =
    if not inputs.button_X then
        player
    else
        case player.rideKey of
            Just n -> { player | rideKey = Just ((n+1) % keyLimit) }
            Nothing -> { player | rideKey = Just 0 }
{-
    let
        switch = inputs.button_X
        newVehicle = Model.nextVehicle player.vehicle
    in
        if not switch then
            player
        else if newVehicle == Model.vehicleBuggy then
            Debug.log "Switch to buggy!" <|
                mapMotion DreamBuggy.welcome { player | vehicle = newVehicle }
        else if newVehicle == Model.vehicleBird then
            Debug.log "Switch to flying!" <|
                mapMotion DreamBird.welcome { player | vehicle = newVehicle }
        else if newVehicle == Model.vehicleLookAt then
            Debug.log "Switch to LookAt!" <|
                mapMotion LookAt.welcome { player | vehicle = newVehicle }
        -- else if newVehicle == vehicleDebug then
        else
            Debug.log "Switch to debug!" <|
                mapMotion DreamDebug.welcome { player | vehicle = newVehicle }
-}

mapMotion : (Model.Motion -> Model.Motion) -> Model.Player -> Model.Player
mapMotion f player = { player | motion = f player.motion }

gravity : Model.EyeLevel -> Float -> Model.Motion -> Model.Motion
gravity eyeLevel dt motion =
  if getY motion.position <= eyeLevel motion.position then motion else
    let v = toRecord motion.velocity
    in
        { motion | velocity = vec3 v.x (v.y - 9.8 * dt) v.z }
