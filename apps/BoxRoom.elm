module BoxRoom exposing (create)

import App exposing (..)
import App.Control exposing (..)
import Appearance exposing (Appearance)
import Body exposing (..)
import Bounding exposing (emplace)
import Bounding.Box exposing (boundingBox)
import Dispatch exposing (..)
import Html exposing (Html)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Orientation
import Task exposing (Task)
import Primitive.Cube as Cube
import Shaders.WorldVertex exposing (Vertex, worldVertex)

import Shaders.Clouds exposing (clouds)
import Shaders.Kintsugi exposing (kintsugi)
import Shaders.SimplePlasma exposing (simplePlasma)

type alias Attributes =
    { dimensions : Vec3
    }


type alias Model =
    { walls : Body
    , floor : Body
    , ceiling : Body
    }


type alias Msg =
    ()


create : Attributes -> ( App, Cmd AppMsg )
create attributes =
    App.create (init attributes)
        { id = always "_room"
        , label = always "Box_Room"
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = noFraming
        , focus = always Nothing
        , overlay = overlay
        , reposition = always identity
        }


init : Attributes -> ( Model, Cmd (CtrlMsg Msg) )
init attributes =
    let
        ( width, height, length ) =
            V3.toTuple attributes.dimensions

        floorCenterPosition =
            vec3 0 0 0

        originPosition =
            vec3 (-width / 2) 0 (-length / 2)

        make appear =
            { anchor = AnchorGround
            , scale = attributes.dimensions
            , position = floorCenterPosition
            , orientation = Orientation.initial
            , appear = appear
            }

        walls = make (Cube.walls worldVertex simplePlasma)
        floor = make (Cube.floor worldVertex kintsugi)
        ceiling = make (Cube.ceiling worldVertex clouds)

        box =
            { position = originPosition
            , dimensions = attributes.dimensions
            }

        ground =
            { bounds = emplace (boundingBox box)
            , elevation = always 0.0
            }

        groundEffect =
            Task.succeed ground
                |> Task.perform (Effect << UpdateGround ())
    in
        ( { walls = walls
          , floor = floor
          , ceiling = ceiling
          }
        , groundEffect
        )


update : CtrlMsg Msg -> Model -> ( Model, Cmd (CtrlMsg Msg) )
update msg model =
    ( model, Cmd.none )


animate : Ground -> Time -> Model -> Model
animate ground dt model =
    model


bodies : Model -> Vec3 -> List Body
bodies model pos =
    [ model.walls, model.floor, model.ceiling ]


overlay : Model -> Html msg
overlay _ =
    Html.text "The room"
