module Position
    exposing
        ( randomPosition
        )

import Here4.Placement as Placement exposing (Placement)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Random

randomPosition : Placement -> Random.Generator Vec3
randomPosition placement =
    let
        fromXZ x z = vec3 x 0 z
        (minX, maxX) = Placement.coordRangeX placement
        (minZ, maxZ) = Placement.coordRangeZ placement
    in
        Random.map2 fromXZ (Random.float minX maxX) (Random.float minZ maxZ)
    
