module Body.Ground exposing (renderGround)

import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

import Util exposing (hslToVec3)

import Shaders.ColorFragment exposing (colorFragment)
import Shaders.WorldVertex exposing (Vertex, worldVertex)

renderGround p =
    let resolution = vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0
        s = p.globalTime
    in
        [entity worldVertex colorFragment groundMesh
            { iResolution=resolution, iGlobalTime=s
            , iLensDistort=p.lensDistort, view=p.viewMatrix }]


-- The mesh for the ground
groundMesh : Mesh Vertex
groundMesh =
  let green = hslToVec3 (degrees 110) 0.48

      topLeft     = { pos = vec3 -20 -1  20, color = green 0.7, coord = vec3 0 0 0 }
      topRight    = { pos = vec3  20 -1  20, color = green 0.4, coord = vec3 0 0 0 }
      bottomLeft  = { pos = vec3 -20 -1 -20, color = green 0.5, coord = vec3 0 0 0 }
      bottomRight = { pos = vec3  20 -1 -20, color = green 0.6, coord = vec3 0 0 0 }
  in
      Triangle [ (topLeft,topRight,bottomLeft), (bottomLeft,topRight,bottomRight) ]