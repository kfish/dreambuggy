module App exposing (..)

import App.Internal as Internal
import Body exposing (..)
import Camera exposing (Framing)
import Control exposing (..)
import Dispatch exposing (..)
import Location exposing (Location)
import Model exposing (PartyKey)
import Task exposing (Task)

type alias App = Internal.App
type alias AppMsg = Internal.AppMsg

type alias CtrlMsg a = Control.CtrlMsg a
type alias EffectMsg = Control.EffectMsg

type alias AppPosition = Internal.AppPosition
type alias Focus = Internal.Focus

type alias PartyKey = Model.PartyKey

create : ( model, Cmd (CtrlMsg msg) ) -> Internal.Animated model (CtrlMsg msg) -> ( App, Cmd AppMsg )
create = Internal.create

createUncontrolled : ( model, Cmd msg ) -> Internal.Animated model msg -> ( App, Cmd AppMsg )
createUncontrolled = Internal.createUncontrolled

appToFocus : Oriented a -> Focus
appToFocus = Internal.appToFocus

orientedToFocus : Oriented a -> Focus
orientedToFocus = Internal.orientedToFocus

noFraming : PartyKey -> model -> Maybe Framing
noFraming _ _ =
    Nothing

teleport : PartyKey -> Location -> Cmd (CtrlMsg a)
teleport partyKey location =
    Task.succeed location |> Task.perform (Effect << RelocateParty () partyKey)
