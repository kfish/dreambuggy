module Sky exposing (create)

import App exposing (..)
import App.Control exposing (..)
import Appearance exposing (Appearance)
import Body exposing (..)
import Dispatch exposing (..)
import Html exposing (Html)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Task exposing (Task)


type alias Model =
    Body


type alias Msg =
    ()


create : Appearance -> ( App, Cmd AppMsg )
create appear =
    App.create (init appear)
        { id = always "sky"
        , label = always "Sky"
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = noFraming
        , focus = always Nothing
        , overlay = overlay
        , reposition = always identity
        }


init : Appearance -> ( Model, Cmd (CtrlMsg Msg) )
init appear =
    ( anchorSky <| resize 300 <| put (vec3 0 0 0) appear, Cmd.none )


update : CtrlMsg Msg -> Model -> ( Model, Cmd (CtrlMsg Msg) )
update msg model =
    case msg of
        Ctrl (Move dp) ->
            ( translate dp model, Cmd.none )

        _ ->
            ( model, Cmd.none )


animate : Ground -> Time -> Model -> Model
animate ground dt body =
    body


bodies : Model -> Vec3 -> List Body
bodies body pos =
    if V3.getY pos > 0 then
        [ body ]
    else
        []


overlay : Model -> Html msg
overlay _ =
    Html.text "A clear blue sky"
