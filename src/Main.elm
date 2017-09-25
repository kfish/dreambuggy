module Main exposing (main)

import Html exposing (Html)
import Here4.Ground exposing (..)
import Here4.Location exposing (..)
import Here4.Navigator exposing (..)
import Here4.Object as Object
import Here4.Object.Attributes exposing (..)
import Here4.Placement exposing (defaultPlacement)
import Here4.Primitive.Cube exposing (skyCube, fireCube, fogMountainsCube, voronoiCube, cubeMesh)
import Here4.Primitive.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Here4.Primitive.Sphere exposing (skySphere, cloudsSphere)
import Here4.Vehicle.DreamBird as DreamBird
import Here4.Vehicle.DreamBuggy as DreamBuggy
import Here4.Vehicle.Walking as Walking
import Math.Vector3 as V3 exposing (Vec3, vec3)
import AddApps exposing (..)
import Boids
import BoxRoom
import Balls
import Demo
import Road
import Shufflepuck
import Sky
import StaticGround
import WaterWalls
import Body.Terrain as Terrain
import Body.Wedge exposing (wedge)
import Position exposing (randomPosition)
import Random


main : Navigator Demo.Flags Demo.Model Demo.Msg
main =
    Demo.create
        [ { id = "world1"
{-
          , label = "Dreambuggy"
          , backgroundColor = rgb 135 206 235
          , apps =
                [ StaticGround.create Terrain.generate
                , WaterWalls.create defaultPlacement
                , Sky.create skySphere

                , ramp (1/40.0) 300 30 (vec3 10 0 -50)

                -- , spiralRoad (1/2.0) 80.0 300 30.0 (vec3 0 0 80)
                , spiralRoad (1/30.0) 5.0 300 4.0 (vec3 30 0 20)
                -- , addApps ( List.repeat 30 (addSomewhere aboveSeaLevel (spiralRoad (1/5.0) 50.0 300 10.0)) )

                , elmLogo

                -- , Balls.create 30

                , addApps
                    [ deltaWedge <| vec3 23 0 12
                    , deltaWedge <| vec3 33 0 12
                    , buggy <| vec3 27 0 43
                    , buggy <| vec3 37 0 43
                    ]

                -- , addApps ( List.repeat 1000 (addRandom (random textureCube)) )
                -- , addApps ( List.repeat 100 (addSomewhere (on ShallowWater) textureCube) )

{-
                , addApps ( List.repeat 100 (addSomewhere (on Beach) hovercraft) )

                , addApps ( List.repeat 100 (addSomewhere (on ShallowWater) boat) )

                -- , addApps ( List.repeat 100 (addRandom (random buggy)) )
                , addApps ( List.repeat 100 (addSomewhere aboveSeaLevel deltaWedge) )
-}

                , addApps ( List.repeat 100 (addAnywhere fireCubePortal) )

                {-
                   , Object.create
                       [ id "clouds-sphere"
                       , label "Clouds Sphere"
                       , position <| vec3 3 10 5
                       , overlay  <| Html.text "Clouds sphere"
                       , object   <| Appearance cloudsSphere (vec3 2 2 2)
                       ]

                   , Object.create
                       [ id "landscape-diamond"
                       , label "Landscape Diamond"
                       , position <| vec3 40 1.5 28
                       , object   <| Appearance fogMountainsDiamond (vec3 2 2 2)
                       ]
                -}

                , Object.create
                    [ id "sky-diamond"
                    , label "Sky Diamond"
                    , position <| vec3 -15 1.5 21
                    , object <| Appearance cloudsDiamond (vec3 2 2 2)
                    , portal <| Remote "world2" (Behind "shufflepuck")
                    ]
                , Boids.create 100
                , Object.create
                    [ id "voronoi-cube"
                    , label "Voronoi Cube"
                    , position <| vec3 10 0 10
                    , object <| Appearance voronoiCube (vec3 1 1 1)
                    , portal <| Local (Become "boids")
                    ]

                {-
                   , Object.create
                       [ id "landscape-cube"
                       , label "Landscape Cube"
                       , position <| vec3 10 1.5 -10
                       , object   <| Appearance fogMountainsCube (vec3 1 1 1)
                       ]
                -}
                ]
          , defaultSelf = avatar 8.0
          }
        , { id = "world2"
-}
          -- , label = "Shufflepuck Club"
          , label = "Mondrian Testing Facility"
          , backgroundColor = rgb 255 255 255
          , apps =
                [ BoxRoom.create
                      [ BoxRoom.dimensions <| vec3 200 30 300
                      , BoxRoom.floor 10.0
                      ]
{-
                , let
                    s =
                        Shufflepuck.default
                  in
                    Shufflepuck.create
                        { s
                            | id = "shufflepuck"
                            , position = vec3 0 0 0
                        }
-}
{-
                , let
                      radius = 5.0
                      spiralUp n =
                          -- vec3 (3.0 * sin (n/10.0)) (n/5.0) (3.0 * cos (n/10.0))
                          vec3 (radius * sin (n/10.0)) (n/30.0) (radius * cos (n/10.0))
                      path =
                          List.map spiralUp (List.map toFloat (List.range 1 300))
                  in
                      Road.create 4.0 path (vec3 0 0.0 0)
-}
                -- , spiralRoad (1/30.0) 5.0 300 4.0 (vec3 0 0 0)
                -- , ramp (1/30.0) 300 4.0 (vec3 -60 1.0 -50)

                --, Road.create 20 [ vec3 0 0 0, vec3 0 5 10 ] (vec3 0 0 0)
                --

                , makeRoad 15 (vec3 -50 0 0)
                    [ straight 10 (vec3 0 0 10)
                    , straight 10 (vec3 0 5 30)
                    , curve 16 0 20 90
                    , straight 10 (vec3 20 0 0)
                    , curve 16 0 20 90
                    , straight 10 (vec3 0 -5 -10)
                    , straight 10 (vec3 0 0 -20)
                    , straight 10 (vec3 0 5 -10)
                    , straight 10 (vec3 0 0 -10)
                    , straight 10 (vec3 0 -5 -10)
                    , straight 10 (vec3 0 0 -30)
                    , straight 10 (vec3 0 5 -5)
                    , straight 10 (vec3 0 0 -10)
                    , straight 10 (vec3 0 -5 -5)
                    , straight 10 (vec3 0 0 -10)
                    , curve 16 0 20 90
                    , straight 10 (vec3 -20 0 0)
                    , curve 16 0 20 90
                    , straightTo 10 (vec3 0 0 0)
                    ]

                , addApps ( List.repeat 30 (addAnywhere textureCube) )

                , Object.create
                    [ id "fire-cube"
                    , label "Fire Cube"
                    , position <| vec3 9 0 -14
                    , object <| Appearance fireCube (vec3 1 1 1)
                    , portal <| Remote "world1" (Facing "fire-cube")
                    ]
                ]
          , defaultSelf = avatar 30 -- 5.7

          {-
             , defaultSelf =
                 Statue.create
                     { label = "Hippy actualization",  position = vec3 30 50 6, appear = cloudsSphere }
          -}
          }
        ]


type alias RoadPoint =
    { position : Vec3
    , forward : Vec3
    }


toRoadPoint v =
    { position = v
    , forward = V3.k
    }


curve : Float -> Float -> Float -> Float
    -> RoadPoint -> (RoadPoint, List RoadPoint)
curve nSegmentsPer360 rise radius degrees =
    let
        nSegments =
            nSegmentsPer360 * abs degrees / 360.0

        sign =
            if degrees < 0 then
                -1
            else
                1

        arc start n =
            let
                right =
                    V3.cross start.forward V3.j
                    |> V3.normalize

                center =
                    V3.scale (sign * radius) right
                    |> V3.sub start.position

                startRadians =
                    atan2 (V3.getX right) (V3.getZ right)

                radians =
                    startRadians + (sign * 2 * pi * n / nSegmentsPer360)

                unitRise =
                    rise / nSegments

                y =
                    n * unitRise
            in
                { position =
                    vec3 (radius * sin radians) y (radius * cos radians)
                    |> V3.add center
                , forward =
                    vec3 (cos radians) unitRise (sin radians) -- close enough
                }
    in
        generatePath nSegments arc


straight : Float -> Vec3 -> RoadPoint -> (RoadPoint, List RoadPoint)
straight segmentLength relativePath  =
    let
        length =
            V3.length relativePath

        nSegments =
            length / segmentLength

        unitPath =
            V3.normalize relativePath

        ramp start n =
            { position =
                V3.scale (n * segmentLength) unitPath
                |> V3.add start.position
            , forward = unitPath
            }
    in
        generatePath nSegments ramp


straightTo : Float -> Vec3 -> RoadPoint -> (RoadPoint, List RoadPoint)
straightTo segmentLength end start =
    straight segmentLength (V3.sub end start.position) start


generatePath : Float -> (RoadPoint -> Float -> RoadPoint) -> RoadPoint -> ( RoadPoint, List RoadPoint )
generatePath nSegments f start =
    let
        nFullSegments =
            floor nSegments

        path =
            List.map (f start) (List.map toFloat (List.range 1 nFullSegments))

        end =
            f start nSegments
    in
        if nSegments - toFloat nFullSegments < 1e3 then
            (end, path)
        else
            (end, path++[end])
 

foldPath : List (RoadPoint -> (RoadPoint, List RoadPoint)) -> RoadPoint -> (RoadPoint, List RoadPoint)
foldPath paths start =
    let
        -- proc : Func -> (RoadPoint, List Vec3) -> (RoadPoint, List Vec3)
        proc f (end0, path0) =
            let
                (end, path) =
                    f end0
            in
                (end, path0 ++ path)
    in
        List.foldl proc (start, []) paths


makeRoad : Float -> Vec3 -> List (RoadPoint -> (RoadPoint, List RoadPoint)) -> ( App, Cmd AppMsg )
makeRoad width pos paths =
    let
        start =
            vec3 0 0 0

        path =
            foldPath paths (toRoadPoint start)
            |> Tuple.second
            |> List.map .position
    in
        Road.create width (start :: path) pos


spiralRoad : Float -> Float -> Int -> Float -> Vec3 -> ( App, Cmd AppMsg )
spiralRoad rise radius length width pos =
    let
        spiralUp n =
            vec3 (radius * sin (n/10.0)) (n * rise) (radius * cos (n/10.0))
        path =
            List.map spiralUp (List.map toFloat (List.range 0 length))
    in
        Road.create width path pos


ramp : Float -> Int -> Float -> Vec3 -> ( App, Cmd AppMsg )
ramp  rise length width pos =
    let
        rampUp n =
            vec3 0 (n * rise) (n/10.0)

        path =
            List.map rampUp (List.map toFloat (List.range 0 length))
    in
        Road.create width path pos


deltaWedge : Vec3 -> ( App, Cmd AppMsg )
deltaWedge pos =
    let
        html =
            Html.div []
                [ Html.h2 []
                    [ Html.text "Delta Wedge" ]
                , Html.text "A manoueverable delta wing with solid plasma anti-gravity thrusters and zero-friction flight surfaces."
                , Html.br [] []
                , Html.hr [] []
                , DreamBird.overlay
                ]
    in
        Object.create
            [ id "wedge"
            , label "Delta Wedge"
            , position pos
            , canFloat True
            , scale <| Scale 7.0
            , overlay <| html
            , object <| Appearance wedge (vec3 1 1 1)
            , vehicle <|
                { drive = DreamBird.drive
                , vehicle =
                    { speed = 20.0
                    , height = 0.7
                    , radius = 1.0
                    }
                }
            ]


random thing = Random.map thing (randomPosition defaultPlacement)

textureCube : Vec3 -> ( App, Cmd AppMsg )
textureCube pos =
    let
        html =
            Html.div []
                [ Html.h2 []
                    [ Html.text "A wooden box" ]
                , Html.text "This highly attractive wooden box doubles as a secret vehicle."
                , Html.br [] []
                , Html.hr [] []
                , DreamBuggy.overlay
                ]
    in
        Object.create
            [ id "crate"
            , label "Wooden crate"
            , position pos
            , canFloat True
            , overlay html
            , object <|
                FlatTexture
                    { mesh = cubeMesh
                    , texturePath = "resources/woodCrate.jpg"
                    }
            , vehicle <|
                { drive = DreamBuggy.drive
                , vehicle =
                    { speed = 40 -- 8.0
                    , height = 1.0
                    , radius = 0.0
                    }
                }
            ]


fireCubePortal : Vec3 -> ( App, Cmd AppMsg )
fireCubePortal pos =
    Object.create
        [ id "fire-cube"
        , label "Fire Cube"
        , position pos
        , object <| Appearance fireCube (vec3 1 1 1)
        , portal <| Remote "world2" (Facing "fire-cube")
        ]


buggy : Vec3 -> ( App, Cmd AppMsg )
buggy pos =
    let
        html =
            Html.div []
                [ Html.h2 []
                    [ Html.text "Buggy" ]
                , Html.br [] []
                , Html.hr [] []
                , DreamBuggy.overlay
                ]
    in
        Object.create
            [ id "buggy"
            , label "Buggy"
            , canFloat False
            , position pos
            , overlay <| html
            , object <|
                Object.texturedObjWith
                    "OffRoad Car/Models/OFF -Road car  3D Models.obj"
                    "textures/elmLogoDiffuse.png"
                    "textures/elmLogoNorm.png"
                    [ offset <| FloorCenter
                    , scale <| Width 1.6
                    , forward <| V3.i
                    ]
            , vehicle <|
                { drive = DreamBuggy.drive
                , vehicle =
                    { speed = 10.0
                    , height = 0.6
                    , radius = 0.0
                    }
                }
            ]


hovercraft : Vec3 -> ( App, Cmd AppMsg )
hovercraft pos =
    let
        html =
            Html.div []
                [ Html.h2 []
                    [ Html.text "Hovercraft" ]
                , Html.br [] []
                , Html.hr [] []
                , DreamBuggy.overlay
                ]
    in
        Object.create
            [ id "hovercraft"
            , label "Hovercraft"
            , canFloat True
            , position pos
            , overlay <| html
            , object <|
                Object.texturedObjWith
                    "meshes/hovercraft1.obj"
                    "textures/elmLogoDiffuse.png"
                    "textures/elmLogoNorm.png"
                    [ offset <| FloorCenter
                    , scale <| Width 1.8
                    , forward <| V3.i
                    ]
            , vehicle <|
                { drive = DreamBuggy.hovercraft
                , vehicle =
                    { speed = 10.0
                    , height = 1.0
                    , radius = 0.0
                    }
                }
            ]

boat : Vec3 -> ( App, Cmd AppMsg )
boat pos =
    let
        html =
            Html.div []
                [ Html.h2 []
                    [ Html.text "Boat" ]
                , Html.br [] []
                , Html.hr [] []
                , DreamBuggy.overlay
                ]
    in
        Object.create
            [ id "boat"
            , label "Boat"
            , canFloat True
            , position pos
            , overlay <| html
            , object <|
                Object.texturedObjWith
                    "meshes/boat1.obj"
                    "textures/elmLogoDiffuse.png"
                    "textures/elmLogoNorm.png"
                    [ offset <| FloorCenter
                    , scale <| Width 1.8
                    , forward <| V3.i
                    ]
            , vehicle <|
                { drive = DreamBuggy.boat
                , vehicle =
                    { speed = 10.0
                    , height = 0.2
                    , radius = 0.0
                    }
                }
            ]

elmLogo : ( App, Cmd AppMsg )
elmLogo =
    let
        html =
            Html.div []
                [ Html.h2 []
                    [ Html.text "Elm Logo" ]
                , Html.br [] []
                , Html.hr [] []
                , DreamBuggy.overlay
                ]
    in
        Object.create
            [ id "elm-logo"
            , label "Elm Logo"
            , canFloat False
            , position <| vec3 38 30 -112
            , overlay <| html

            -- , forward  <| V3.negate V3.k
            , object <|
                Object.texturedObjWith
                    "meshes/elmLogo.obj"
                    "textures/elmLogoDiffuse.png"
                    "textures/elmLogoNorm.png"
                    [ offset <| Center -- WorldSpace 0 20 0
                    , scale <| Height 20
                    ]
            , vehicle <|
                { drive = DreamBuggy.drive
                , vehicle =
                    { speed = 8.0
                    , height = 1.2
                    , radius = 1.0
                    }
                }
            ]


avatar : Float -> ( App, Cmd AppMsg )
avatar speed =
    let
        html =
            Html.div []
                [ Html.h2 []
                    [ Html.text "Avatar" ]
                , Html.br [] []
                , Html.hr [] []
                , Walking.overlay
                ]
    in
        Object.create
            [ id "avatar"
            , label "Walking"
            , position <| vec3 0 0 0
            , overlay <| html
            , object <|
                Object.reflectiveObjWith
                    "meshes/suzanne.obj"
                    "textures/elmLogoDiffuse.png"
                    [ offset <| FloorCenter
                    , scale <| Height 0.6
                    ]
            , vehicle <|
                { drive = Walking.drive
                , vehicle =
                    { speed = speed
                    , height = 1.0
                    , radius = 0.5
                    }
                }
            ]
