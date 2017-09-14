module Main exposing (main)

import Html exposing (Html)
import Here4.Ground exposing (..)
import Here4.Location exposing (..)
import Here4.Navigator exposing (..)
import Here4.Object as Object
import Here4.Object.Attributes exposing (..)
import Here4.Orientation
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
          , label = "Dreambuggy"
          , backgroundColor = rgb 135 206 235
          , apps =
                [ StaticGround.create Terrain.generate
                , WaterWalls.create defaultPlacement
                , Sky.create skySphere
                , elmLogo

                -- , Balls.create 30

                , addApps
                    [ deltaWedge <| vec3 23 0 12
                    , deltaWedge <| vec3 33 0 12
                    , buggy <| vec3 27 0 43
                    , buggy <| vec3 37 0 43
                    ]

                -- , addApps ( List.repeat 1000 (addRandom (random textureCube)) )
                , addApps ( List.repeat 100 (addSomewhere (on ShallowWater) textureCube) )

                , addApps ( List.repeat 100 (addSomewhere (on Beach) hovercraft) )

                , addApps ( List.repeat 100 (addSomewhere (on ShallowWater) boat) )

                -- , addApps ( List.repeat 100 (addRandom (random buggy)) )
                , addApps ( List.repeat 100 (addSomewhere aboveSeaLevel deltaWedge) )

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
          , label = "Shufflepuck Club"
          , backgroundColor = rgb 255 255 255
          , apps =
                [ BoxRoom.create
                      [ BoxRoom.dimensions <| vec3 20 10 30
                      ]
                , let
                    s =
                        Shufflepuck.default
                  in
                    Shufflepuck.create
                        { s
                            | id = "shufflepuck"
                            , position = vec3 0 0 0
                        }
                , Object.create
                    [ id "fire-cube"
                    , label "Fire Cube"
                    , position <| vec3 9 0 -14
                    , object <| Appearance fireCube (vec3 1 1 1)
                    , portal <| Remote "world1" (Facing "fire-cube")
                    ]
                ]
          , defaultSelf = avatar 5.7

          {-
             , defaultSelf =
                 Statue.create
                     { label = "Hippy actualization",  position = vec3 30 50 6, appear = cloudsSphere }
          -}
          }
        ]


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
                { drive = DreamBuggy.boat
                , vehicle =
                    { speed = 8.0
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
