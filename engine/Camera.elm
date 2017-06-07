module Camera exposing (..)

import Math.Vector3 as V3 exposing (Vec3)

import Orientation exposing (Orientation)

{-
http://www.empireonline.com/movies/features/film-studies-101-camera-shots-styles/
https://www.videomaker.com/article/c10/14221-camera-movement-techniques-tilt-pan-zoom-pedestal-dolly-and-truck
-}

type Shot = POV | Tracking

type alias Camera =
    { position : Vec3
    , orientation : Orientation
    }