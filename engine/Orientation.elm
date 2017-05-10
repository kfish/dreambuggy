module Orientation exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Math.Quaternion as Qn

type alias Orientation = Qn.Quaternion

initial : Orientation
initial = Qn.unit

fromRollPitchYaw : (Float, Float, Float) -> Orientation
fromRollPitchYaw = Qn.fromEuler

toRollPitchYaw : Orientation -> (Float, Float, Float)
toRollPitchYaw = Qn.toEuler

followedBy : Orientation -> Orientation -> Orientation
followedBy = Qn.hamilton

rotateBodyV : Orientation -> Vec3 -> Vec3
-- rotateBodyV = Qn.vrotate
rotateBodyV o = Qn.vrotate (Qn.negate o) 

rotateLabV : Orientation -> Vec3 -> Vec3
-- rotateLabV o = Qn.vrotate (Qn.negate o) 
rotateLabV = Qn.vrotate

