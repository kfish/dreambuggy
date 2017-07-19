module App.Internal exposing
    (Animated, App, AppMsg, AppPosition, create, createUncontrolled, Focus, animate, bodies, reposition, id, label, overlay, framing, focus, update, appToFocus, orientedToFocus)


import App.Control exposing (..)
import Html exposing (Html)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Math.Matrix4 exposing (Mat4)
import Orientation exposing (Orientation)
import Time exposing (Time)
import WebGL exposing (Entity)
import Appearance exposing (..)
import Body exposing (..)
import Camera.Types exposing (Framing, Shot)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)
import Ground exposing (Ground)
import Model exposing (PartyKey)


type alias Animated model msg =
    { id : model -> String
    , label : model -> String
    , update : msg -> model -> ( model, Cmd msg )
    , bodies : model -> (Vec3 -> List Body)
    , animate : Ground -> Time -> model -> model
    , framing : PartyKey -> model -> Maybe Framing
    , focus : model -> Maybe Focus
    , overlay : model -> Html msg
    , reposition : Maybe AppPosition -> model -> model
    }


type alias AppPosition =
    { position : Vec3
    , orientation : Orientation
    }


type alias AppModel =
    Dynamic


type alias AppMsg =
    CtrlMsg Dynamic


type alias App =
    { methods : Animated AppModel AppMsg
    , model : AppModel
    }


msgUnpack : CtrlMsg Dynamic -> CtrlMsg a
msgUnpack msg =
    case msg of
        Self m ->
            Self (Dynamic.unpack m)

        Ctrl c ->
            Ctrl c

        Effect e ->
            Effect e


msgPack : CtrlMsg a -> CtrlMsg Dynamic
msgPack msg =
    case msg of
        Self m ->
            Self (Dynamic.pack m)

        Ctrl c ->
            Ctrl c

        Effect e ->
            Effect e


packInit : ( model, Cmd msg ) -> ( AppModel, Cmd AppMsg )
packInit ( x, cmd ) =
    ( Dynamic.pack x, Cmd.map (Self << Dynamic.pack) cmd )


packId : (model -> String) -> AppModel -> String
packId f dyn =
    f (Dynamic.unpack dyn)


packLabel : (model -> String) -> AppModel -> String
packLabel f dyn =
    f (Dynamic.unpack dyn)


packUpdate : (CtrlMsg msg -> model -> ( model, Cmd (CtrlMsg msg) )) -> AppMsg -> AppModel -> ( AppModel, Cmd AppMsg )
packUpdate f msg dyn =
    let
        ( newModel, newCmdMsg ) =
            f (msgUnpack msg) (Dynamic.unpack dyn)
    in
        ( Dynamic.pack newModel, Cmd.map msgPack newCmdMsg )


packAnimate : (Ground -> Time -> model -> model) -> Ground -> Time -> AppModel -> AppModel
packAnimate f ground dt dyn =
    Dynamic.pack (f ground dt (Dynamic.unpack dyn))


packBodies : (a -> (Vec3 -> List Body)) -> AppModel -> (Vec3 -> List Body)
packBodies f dyn =
    f (Dynamic.unpack dyn)


packFraming : (PartyKey -> a -> Maybe Framing) -> PartyKey -> AppModel -> Maybe Framing
packFraming f partyKey dyn =
    f partyKey (Dynamic.unpack dyn)


packFocus : (a -> Maybe Focus) -> AppModel -> Maybe Focus
packFocus f dyn =
    f (Dynamic.unpack dyn)


packOverlay : (a -> Html (CtrlMsg msg)) -> AppModel -> Html AppMsg
packOverlay f dyn =
    Html.map msgPack <| f (Dynamic.unpack dyn)


packReposition : (Maybe AppPosition -> model -> model) -> Maybe AppPosition -> AppModel -> AppModel
packReposition f pos dyn =
    Dynamic.pack (f pos (Dynamic.unpack dyn))


packMethods : Animated model (CtrlMsg msg) -> Animated AppModel AppMsg
packMethods { id, label, update, animate, bodies, framing, focus, overlay, reposition } =
    { id = packId id
    , label = packLabel label
    , update = packUpdate update
    , animate = packAnimate animate
    , bodies = packBodies bodies
    , framing = packFraming framing
    , focus = packFocus focus
    , overlay = packOverlay overlay
    , reposition = packReposition reposition
    }



-- | Create an app


create : ( model, Cmd (CtrlMsg msg) ) -> Animated model (CtrlMsg msg) -> ( App, Cmd AppMsg )
create ( model, msg ) methods =
    ( { methods = packMethods methods
      , model = Dynamic.pack model
      }
    , Cmd.map msgPack msg
    )



-- | Create an app that ignores control messages


createUncontrolled : ( model, Cmd msg ) -> Animated model msg -> ( App, Cmd AppMsg )
createUncontrolled ( model, msg ) methods =
    create
        ( model, Cmd.map Self msg )
        { methods
            | update = updateSelf methods.update
            , overlay = overlaySelf methods.overlay
        }


overlaySelf : (a -> Html msg) -> a -> Html (CtrlMsg msg)
overlaySelf f model =
    Html.map Self (f model)



-- | Update helper for apps with no children


updateSelf :
    (msg -> model -> ( model, Cmd msg ))
    -> CtrlMsg msg
    -> model
    -> ( model, Cmd (CtrlMsg msg) )
updateSelf f msg model =
    case msg of
        Self selfMsg ->
            let
                ( newModel, newMsg ) =
                    f selfMsg model
            in
                ( newModel, Cmd.map Self newMsg )

        _ ->
            ( model, Cmd.none )



{-
   ----------------------------------------------------------------------
   -- Debugging: noop App

   update0 _ m = (m, Cmd.none)
   animate0 dt t = t
   bodies0 _ = []
   methods0 = { update = update0, animate = animate0, bodies = bodies0 }

   createApp0 : (model, Cmd msg) -> App
   createApp0 (model, msg) =
       { methods = methods0
       , model = Dynamic.pack model
       }
   ----------------------------------------------------------------------
-}


id : App -> String
id { methods, model } =
    methods.id model


label : App -> String
label { methods, model } =
    methods.label model


update : AppMsg -> App -> ( App, Cmd AppMsg )
update msg { methods, model } =
    let
        ( newModel, newCmdMsg ) =
            methods.update msg model
    in
        ( { methods = methods, model = newModel }, newCmdMsg )



-- animate : Ground -> Time -> App -> (App, Cmd AppMsg)


animate : Ground -> Time -> App -> App
animate ground dt { methods, model } =
    let
        newModel =
            methods.animate ground dt model
    in
        { methods = methods, model = newModel }


bodies : App -> (Vec3 -> List Body)
bodies { methods, model } =
    methods.bodies model


reposition : Maybe AppPosition -> App -> App
reposition pos { methods, model } =
    { methods = methods, model = methods.reposition pos model }


framing : PartyKey -> App -> Maybe Framing
framing partyKey { methods, model } =
    methods.framing partyKey model


focus : App -> Maybe Focus
focus { methods, model } =
    methods.focus model


overlay : App -> Html AppMsg
overlay { methods, model } =
    methods.overlay model



-- TODO: focus on a plane/surface/controls


type alias Focus =
    { position : Vec3
    }


appToFocus : Oriented a -> Focus
appToFocus body =
    { position = body.position }


orientedToFocus : Oriented a -> Focus
orientedToFocus x =
    { position = x.position }


