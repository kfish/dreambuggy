module StaticGround exposing (create)

import Task exposing (Task)
import Time exposing (Time)

import App exposing (App, AppMsg, Focus)
import Body exposing (Body)
import Control exposing (..)
import Dispatch exposing (..)
import Ground exposing (Ground)

type alias Model = List Body

type Msg
    = GroundGenerated (Ground, List Body)

create : (((Ground, List Body) -> Msg) -> Cmd Msg)
    -> (App, Cmd AppMsg)
create makeGround = App.create (init makeGround)
    { update = update
    , animate = animate
    , bodies = bodies
    , camera = always Nothing
    , focus = always Nothing
    }

init : (((Ground, List Body) -> Msg) -> Cmd Msg)
    -> (Model, Cmd (CtrlMsg Msg))
init makeGround =
    ( []
    , Cmd.map Self (makeGround GroundGenerated)
    )

update : CtrlMsg Msg -> Model
    -> (Model, Cmd (CtrlMsg Msg))
update msg model = case msg of
    Self (GroundGenerated (ground, bodies)) ->
            ( bodies
            , Task.succeed ground
                |> Task.perform (Effect << UpdateGround)
            )
    _ -> (model, Cmd.none)

animate : Time -> Model -> Model
animate dt model = model

bodies : Model -> List Body
bodies = identity
