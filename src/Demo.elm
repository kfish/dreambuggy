module Demo exposing (create, Flags, Model, Msg)

import Here4.App exposing (..)
import Here4.Control exposing (..)
import Here4.Dispatch exposing (..)
import Here4.Navigator.Control exposing (NavMsg)
import Here4.Model as Model
import Here4.World as World exposing (..)
import Mouse
import Ports
import Task


type alias Flags =
    { movement : MouseMovement
    , isLocked : Bool
    }

{-| This type is returned by the fullscreen JS api in PointerLock.js
for mouse movement
-}
type alias MouseMovement =
    ( Int, Int )


type DemoMsg
    = MouseMove MouseMovement
    | LockRequest Bool
    | LockUpdate Bool


type alias Msg =
    WorldMsg (NavMsg DemoMsg)


type alias DemoModel =
    { wantToBeLocked : Bool
    , isLocked : Bool
    }


type alias Model =
    Multiverse DemoModel


create :
    List World.Attributes
    -> Program Flags (Model.Model Model Msg) (Model.Msg Msg)
create attributes =
    World.create init update subscriptions attributes


init : Flags -> ( DemoModel, Cmd (NavMsg DemoMsg) )
init flags =
    ( { wantToBeLocked = True
      , isLocked = flags.isLocked
      }
    , Cmd.none )

update : NavMsg DemoMsg -> DemoModel -> ( DemoModel, Cmd (NavMsg DemoMsg) )
update msg model =
    case msg of
        Self (MouseMove movement) ->
            let
                inputs =
                    mouseToInputs movement Model.noInput

                cmd =
                    Task.succeed inputs
                    |> Task.perform (Effect << Model.ProvideInputs)
            in
                ( model, cmd )

        Self (LockRequest wantToBeLocked) ->
            ( { model | wantToBeLocked = wantToBeLocked }
            , if model.wantToBeLocked == model.isLocked then
                Cmd.none
              else if model.wantToBeLocked then
                Ports.requestPointerLock ()
              else
                Ports.exitPointerLock ()
            )

        Self (LockUpdate isLocked) ->
            ( { model | isLocked = isLocked }, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Multiverse DemoModel -> Sub (NavMsg DemoMsg)
subscriptions model =
    [ Ports.isLocked (\x -> Self (LockUpdate x))
    ]
        ++ (if model.state.isLocked then
                [ Ports.movement (\x -> Self (MouseMove x)) ]
            else
                [ Mouse.clicks (\_ -> Self (LockRequest True)) ]
           )
        |> Sub.batch


mouseToInputs : Model.MouseMovement -> Model.Inputs -> Model.Inputs
mouseToInputs ( mx, my ) inputs =
    { inputs | mx = 0.5 * toFloat mx, my = -0.5 * toFloat my }
