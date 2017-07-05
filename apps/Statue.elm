module Statue exposing (create)

import Html exposing (Html)
import Html.Attributes as Html
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Task exposing (Task)
import Time exposing (Time)
import App exposing (App, AppMsg, AppPosition, Focus, appToFocus)
import Appearance exposing (Appearance)
import Body exposing (..)
import Camera exposing (..)
import Camera.Util as Camera
import Control exposing (CtrlMsg)
import Dispatch exposing (..)
import Ground exposing (Ground)
import Model exposing (Motion)
import Orientation
import Camera.DollyArc as Camera


type alias Model =
    { label : String
    , body : Moving Body
    }


type alias Msg =
    ()


create : String -> Vec3 -> Appearance -> ( App, Cmd AppMsg )
create label pos appear =
    App.create (init label pos appear)
        { label = always label
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = framing
        , focus = focus
        , overlay = overlay
        , reposition = reposition
        }


init : String -> Vec3 -> Appearance -> ( Model, Cmd (CtrlMsg Msg) )
init label pos appear =
    ( { label = label
      , body =
            { anchor = AnchorGround
            , scale = vec3 1 1 1
            , position = pos
            , orientation = Orientation.initial
            , appear = appear
            , velocity = vec3 0 0 0
            }
      }
    , Cmd.none
    )


update : CtrlMsg Msg -> Model -> ( Model, Cmd (CtrlMsg Msg) )
update msg model =
    case msg of
        Ctrl (Control.Move dp) ->
            ( { model | body = translate dp model.body }, Cmd.none )

        _ ->
            ( model, Cmd.none )


animate : Ground -> Time -> Model -> Model
animate ground dt model =
    let
        setElevation pos = V3.setY (1.8 + ground.elevation pos) pos
        onGround body = { body | position = setElevation body.position }
    in
        { model | body = onGround model.body }


bodies : Model -> List Body
bodies model =
    [ toBody model.body ]

reposition : Maybe AppPosition -> Model -> Model
reposition mPos model =
    let
        setPos pos x = { x | position = pos.position, orientation = pos.orientation }

        behind pos =
            let
                dir = Orientation.rotateLabV pos.orientation V3.k
            in
                { pos | position = V3.sub pos.position dir }
    in
        case mPos of
            Just pos ->
                { model | body = setPos pos model.body
                }
            Nothing ->
                model

framing : Model -> Maybe Framing
framing model =
    Just (Camera.framing model.body)


focus : Model -> Maybe Focus
focus model =
    Just (appToFocus model.body)

overlay : Model -> Html msg
overlay model =
    let
        textLeft =
            Html.style [ ( "text-align", "left" ) ]
    in
        Html.div []
            [ Html.h2 []
                [ Html.text model.label ]
            , Html.text "A statue"
            ]
