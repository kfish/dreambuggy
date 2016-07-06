module Orientation exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
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

-- TODO: translate to/from flight dynamics / our world coordinates
-- ie. our quaternions work on X=ahead, Y=right, Z=down (RHS)
-- but our world is X=right, Y=up, Z=ahead (LHS)

-- TODO: PLAYTEST THIS!

rotateBodyV : Orientation -> Vec3 -> Vec3
-- rotateBodyV = Qn.vrotate
rotateBodyV = Qn.worldVRotate

rotateLabV : Orientation -> Vec3 -> Vec3
-- rotateLabV o = Qn.vrotate (Qn.negate o) 
rotateLabV o = Qn.worldVRotate (Qn.negate o) 
