module WaterWalls exposing (create)

import App exposing (..)
import App.Control exposing (..)
import Appearance exposing (Appearance)
import Body exposing (..)
import Bounding exposing (emplace)
import Bounding.Box exposing (boundingBox)
import Dispatch exposing (..)
import Html exposing (Html)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector4 as V4 exposing (vec4)
import Orientation
import Placement exposing (Placement)
import Task exposing (Task)
import Primitive.Cube as Cube
import Shaders.WorldVertex exposing (Vertex, worldVertex)
import Shaders.ColorFragment exposing (..)
import Shaders.NoiseVertex exposing (..)

import Shaders.Clouds exposing (clouds)
import Shaders.Kintsugi exposing (kintsugi)
import Shaders.SimplePlasma exposing (simplePlasma)

import Util exposing (hslToVec3)

type alias Attributes =
    { dimensions : Vec3
    }


type alias Model =
    { walls : Body
    , floor : Body
    , hardWalls : Body
    , seaLevel : Float
    }


type alias Msg =
    ()


create : Placement -> ( App, Cmd AppMsg )
create placement =
    App.create (init placement)
        { id = always "_water_walls"
        , label = always "Water Walls"
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = noFraming
        , focus = always Nothing
        , overlay = overlay
        , reposition = always identity
        }


init : Placement -> ( Model, Cmd (CtrlMsg Msg) )
init placement =
    let
        seaLevel =
            -- see terrain/Body/Terrain.elm, 1.0 is the height passed to NoiseSurfaceVertex
            -- 0.085 * placement.yMult
            0.125 * placement.yMult

        x0 =
            placement.xOffset

        x1 =
            x0 + toFloat placement.bigSide * placement.xDelta

        xSide =
            x1 - x0

        xCenter =
           (x0 + x1) / 2.0

        z0 =
            placement.zOffset

        z1 =
            z0 + toFloat placement.bigSide * placement.zDelta

        zSide =
            z1 - z0

        zCenter =
           (z0 + z1) / 2.0


        -- How far the box extends below 0
        depth =
            placement.yMult - placement.yOffset

        -- height of the box
        height =
            depth + seaLevel

        dimensions =
            vec3 xSide height zSide

        floorCenterPosition =
            vec3 xCenter (-depth) zCenter

        followPlayer appear =
            { anchor = AnchorElevation 0
            , scale = vec3 120 height 120
            , position = floorCenterPosition
            , orientation = Orientation.initial
            , appear = appear
            }

        hard appear =
            { anchor = AnchorGround
            , scale = dimensions
            , position = floorCenterPosition
            , orientation = Orientation.initial
            , appear = appear
            }

        sea =
            hslToVec3 (degrees 190) 0.8 ((abs (0.3 / 10) + 0.1) * 1)

        alpha1 v0 =
            let
                v =
                    V3.toRecord v0
            in
                V4.fromTuple ( v.x, v.y, v.z, 1.0 )

        toNoiseVertex v =
            { pos = v.pos
            , coord = V3.scale 10 v.coord
            , color = alpha1 sea -- vec4 0.1 0.1 0.7 1.7
            , smoothing = 0.1
            , textureScale = 1.0
            , timeScale = 0.1
            } 

        walls = followPlayer (Cube.wallsWith toNoiseVertex noiseVertex noiseColorFragment)

        floor = followPlayer (Cube.floorWith toNoiseVertex noiseVertex noiseColorFragment)

        hardWalls = hard (Cube.wallsWith toNoiseVertex noiseVertex noiseColorFragment)
    in
        ( { walls = walls
          , floor = floor
          , hardWalls = hardWalls
          , seaLevel = seaLevel
          }
        , Cmd.none
        )


update : CtrlMsg Msg -> Model -> ( Model, Cmd (CtrlMsg Msg) )
update msg model =
    ( model, Cmd.none )


animate : Ground -> Time -> Model -> Model
animate ground dt model =
    model


bodies : Model -> Vec3 -> List Body
bodies model pos =
    if V3.getY pos <= model.seaLevel then
        [ model.walls, model.floor, model.hardWalls ]
        -- [ model.hardWalls ]
    else
       []


overlay : Model -> Html msg
overlay _ =
    Html.text "Water walls"
