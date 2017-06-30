module Camera exposing (..)

import Math.Vector3 as V3 exposing (Vec3)

import Ground exposing (Ground)
import Orientation exposing (Orientation)

{-
http://www.empireonline.com/movies/features/film-studies-101-camera-shots-styles/
https://www.videomaker.com/article/c10/14221-camera-movement-techniques-tilt-pan-zoom-pedestal-dolly-and-truck
-}

type alias Input =
    { x : Float
    , y : Float
    , dt : Float
    }

type alias Shot =
    { label : String
    , init : Ground -> Camera -> Camera
    , shoot : Ground -> Input -> Target -> Camera -> Camera
    }

type alias Target =
    { position : Vec3
    , orientation : Orientation
    , velocity : Vec3
    }

type alias Framing =
    { target : Target
    , pov : Camera
    }

type alias Camera =
    { position : Vec3
    , orientation : Orientation
    , target : Target
    , fovy : Float
    }
