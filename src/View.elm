module View exposing (view)

import Color exposing (black, white)
import FontAwesome
-- import Html
import Html exposing (Html, text, div, p, span)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as M4
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (..)
import Random
import Time exposing (Time)
import WebGL
import Window

import Array2D exposing (Array2D)
import Model exposing (Model, Msg)
import Orientation
import Thing exposing (..)
-- import Model
import Things.Cube exposing (textureCube, fireCube, fogMountainsCube, voronoiCube)
import Things.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Things.Sphere exposing (cloudsSphere)
import Things.Terrain exposing (Terrain)
import Things.Ground exposing (renderGround)
import Things.Surface2D exposing (Placement, defaultPlacement)
import Things.Terrain as Terrain

import Things.BFly exposing (bfly)
import Shaders.VoronoiDistances exposing (voronoiDistances)

{-| Generate a View from a Model
-}
view : Model -> Html Msg
view model =
    case (model.maybeWindowSize, model.maybeTexture, model.maybeTerrain) of
        (Nothing, _, _) -> text ""
        (_, Nothing, _) -> text ""
        (_, _, Nothing) -> text ""
        (Just windowSize, Just texture, Just terrain) ->
            layoutScene windowSize texture terrain model

layoutScene : Window.Size -> WebGL.Texture -> Terrain -> Model.Model-> Html Msg
layoutScene windowSize texture terrain model =
    if model.person.cameraVR then
        layoutSceneVR windowSize texture terrain model
    else if model.numPlayers == 2 then
        layoutScene2 windowSize texture terrain model
    else
        layoutScene1 windowSize texture terrain model

layoutScene1 : Window.Size -> WebGL.Texture -> Terrain -> Model.Model-> Html Msg
layoutScene1 windowSize texture terrain model =
    div
        [ style
            [ ( "width", toString width ++ "px" )
            , ( "height", toString height ++ "px" )
            , ( "backgroundColor", "rgb(135, 206, 235)" )
            ]
        ]
        [ WebGL.toHtml
            [ width windowSize.width
            , height windowSize.height
            , style [ ( "display", "block" )
                    , ( "position", "absolute" )
                    , ( "top", "0px" )
                    , ( "left", "0px" )
                    , ( "right", "0px" )
                    ]
            ]
            (renderWorld Model.OneEye windowSize texture terrain model model.person)
        , hud model.person 0 0
        ]

layoutScene2 : Window.Size -> WebGL.Texture -> Terrain -> Model.Model-> Html Msg
layoutScene2 windowSize texture terrain model =
    let w2 = windowSize.width // 2 in
    div
        [ style
            [ ( "width", toString windowSize.width ++ "px" )
            , ( "height", toString windowSize.height ++ "px" )
            , ( "backgroundColor", "rgb(135, 206, 235)" )
            ]
        ]
        [ span [] [
            div []
            [ WebGL.toHtml
              [ width w2
              , height windowSize.height
              , style [ ( "display", "block" )
                      , ( "float", "left" )
                      , ( "position", "absolute" )
                      , ( "top", "0px" )
                      , ( "left", "0px" )
                      , ( "right", toString w2 ++ "px" )
                      , ( "border", "0px" )
                      , ( "padding", "0px" )
                      ]
              ]
              (renderWorld Model.OneEye windowSize texture terrain model model.person)
            , hud model.person 0 w2
            ]
          , div []
            [ WebGL.toHtml
              [ width w2
              , height windowSize.height
              , style [ ( "display", "block" )
                      , ( "float", "right" )
                      , ( "position", "absolute" )
                      , ( "top", "0px" )
                      , ( "left", toString w2 ++ "px" )
                      , ( "right", "0px" )
                      , ( "border", "0px" )
                      , ( "padding", "0px" )
                      ]
              ]
              (renderWorld Model.OneEye windowSize texture terrain model model.player2)
            , hud model.player2 w2 0
            ]
          ]
        ]

layoutSceneVR : Window.Size -> WebGL.Texture -> Terrain -> Model.Model-> Html Msg
layoutSceneVR windowSize texture terrain model =
    div
        [ style
            [ ( "width", toString windowSize.width ++ "px" )
            , ( "height", toString windowSize.height ++ "px" )
            , ( "backgroundColor", "rgb(135, 206, 235)" )
            ]
        ]
        [ WebGL.toHtml
            [ width (windowSize.width//2)
            , height windowSize.height
            , style [ ( "display", "block" )
                    , ( "float", "left" )
                    ]
            ]
            (renderWorld Model.LeftEye windowSize texture terrain model model.person)
        , WebGL.toHtml
            [ width (windowSize.width//2)
            , height windowSize.height
            , style [ ( "display", "block" )
                    , ( "float", "right" )
                    ]
            ]
            (renderWorld Model.RightEye windowSize texture terrain model model.person)
        ]

translateP position p = { p | viewMatrix = M4.translate position p.viewMatrix }

mapApply : List (a -> List b) -> a -> List b
mapApply fs x = List.concat <| List.map (\f -> f x) fs

place : Float -> Float -> Float -> Thing -> Thing
place x y z (Thing _ o s) = Thing (vec3 x y z) o s

orient : Thing -> See
orient (Thing position orientation see) =
    let z_axis = vec3 0 0 1
        rot_angle = 0 - acos (dot orientation z_axis)
        rot_axis = normalize (cross orientation z_axis)
    in
        tview (M4.translate position) << tview (M4.rotate rot_angle rot_axis) <| see

eyeOffset : Model.Person -> Model.Eye -> Vec3
eyeOffset person eye =
    if eye == Model.LeftEye then
        Orientation.rotateLabV person.orientation (vec3 (-0.04) 0 0)
    else if eye == Model.RightEye then
        Orientation.rotateLabV person.orientation (vec3 0.04 0 0)
    else
        vec3 0 0 0

aboveTerrain : Model.EyeLevel -> Vec3 -> Vec3
aboveTerrain eyeLevel pos =
    let
        p = toRecord pos
        e = eyeLevel pos
    in
        if p.y < e then vec3 p.x e p.z else pos

{-| Set up 3D world
-}
renderWorld : Model.Eye -> Window.Size -> WebGL.Texture -> Terrain -> Model.Model -> Model.Person -> List WebGL.Renderable
renderWorld eye windowSize texture terrain model person =
    let
        -- placement = defaultPlacement
        eyeLevel pos = Model.eyeLevel + Terrain.elevation terrain pos
        lensDistort = if person.cameraVR then 0.85 else 0.9

        p = { cameraPos = Terrain.bounds terrain (aboveTerrain eyeLevel person.pos)
            , viewMatrix = perspective windowSize person eye
            , globalTime = model.lifetime
            , windowSize = windowSize
            , lensDistort = lensDistort
            , measuredFPS = 30.0
            }

        boidThings = List.map extractThing model.boids
        ballThings = List.map extractThing model.balls
        things = terrain.groundMesh ++ terrain.waterMesh ++ boidThings ++ ballThings
        seeThings = mapApply (List.map orient things)

        worldObjects = List.concat
            [ fogMountainsDiamond (translateP (vec3 0 1.5 0) p)
            , cloudsDiamond (translateP (vec3 5 1.5 1) p)
            , cloudsSphere (translateP (vec3 3 10 5) p)
            , voronoiCube (translateP (vec3 10 0 10) p)
            , fireCube (translateP (vec3 -10 0 -10) p)
            , fogMountainsCube (translateP (vec3 10 1.5 -10) p)
            , textureCube texture (translateP (vec3 -2 0 -17) p)
            -- , renderGround p
            ]
    in
        seeThings p ++ worldObjects

{-| Calculate the viewer's field of view
-}
perspective : Window.Size -> Model.Person -> Model.Eye -> Mat4
perspective { width, height } person eye =
    M4.mul (M4.makePerspective 45 (toFloat width / toFloat height) 0.01 100)
        (M4.makeLookAt (person.cameraPos `add` eyeOffset person eye)
                       (person.pos `add` (scale 3 (Model.direction person)))
                       person.cameraUp)

hud: Model.Person -> Int -> Int -> Html Msg
hud person left right =
    let
        vehicleName = if person.vehicle == Model.vehicleBird then
                          "Dreambird"
                      else if person.vehicle == Model.vehicleBuggy then
                           "Dreambuggy"
                      else "DreamDebug"
        wher = if person.cameraInside then "Inside" else "Outside"
    in div
       [ style
           [ ( "position", "absolute" )
           , ( "font-family", "Verdana, Geneva, sans-serif" )
           , ( "text-align", "center" )
           , ( "left", toString left ++ "px" )
           , ( "right", toString right ++ "px" )
           , ( "top", "0px" )
           , ( "border", "0px" )
           , ( "padding", "0px" )
           , ( "background-color", "rgba(0,0,0,0.5)" )
           , ( "color", "#fff" )
           , ( "font-size", "xx-large" )
           , ( "text-shadow", "1px 0 0 #000, 0 -1px 0 #000, 0 1px 0 #000, -1px 0 0 #000" )
           , ( "z-index", "1" )
           ]
       ]
       [
           span []
           [ Html.text vehicleName
           , Html.text " "
           , FontAwesome.diamond white 20
           , Html.text " "
           , Html.text wher
           ]
       ]

enterMsg : List (Html Msg)
enterMsg = message "Click to go full screen and move your head with the mouse."

exitMsg : List (Html Msg)
exitMsg = message "Press <escape> to exit full screen."

message : String -> List (Html Msg)
message msg =
    [ p [] [ Html.text "Use gamepad, arrows or WASD keys to move." ]
    , p [] [ Html.text msg ]
    ]
