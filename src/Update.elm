module Update (step) where

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)

import Array2D exposing (Array2D)
import Model
import Things.Terrain as Terrain

type alias EyeLevel = Vec3 -> Float

step : Array2D Float -> Model.Inputs -> Model.Person -> Model.Person
step terrain inputs person =
    case inputs of
      Model.Mouse movement -> turn movement person
      Model.TimeDelta isJumping directions dt ->
        let 
            eyeLevel pos = Model.eyeLevel + Terrain.elevation terrain pos
        in
{-
          person |> fly directions
                 |> physics eyeLevel dt
-}
          person |> walk eyeLevel directions
                 |> jump eyeLevel isJumping
                 |> gravity eyeLevel dt
                 |> physics eyeLevel dt

flatten : Vec3 -> Vec3
flatten v =
    let r = toRecord v
    in  normalize (vec3 r.x 0 r.z)

turn : (Int,Int) -> Model.Person -> Model.Person
turn (dx,dy) person =
    -- let yo x = toFloat (clamp -30 30 x) / 500
    let yo x = toFloat x / 500
        h' = person.horizontalAngle + yo dx
        v' = person.verticalAngle   - yo dy
    in
        { person | horizontalAngle = h'
                 -- , verticalAngle = clamp (degrees -90) (degrees 90) v'
                 , verticalAngle = v'
        }

fly : { x:Int, y:Int } -> Model.Person -> Model.Person
fly directions person =
    let moveDir = normalize (Model.direction person)
        strafeDir = transform (makeRotate (degrees -90) j) moveDir

        move = V3.scale (8.0 * toFloat directions.y) moveDir
        strafe = V3.scale (8.0 * toFloat directions.x) strafeDir
    in
        { person | velocity = adjustVelocity (move `add` strafe) }

walk : EyeLevel -> { x:Int, y:Int } -> Model.Person -> Model.Person
walk eyeLevel directions person =
  -- if getY person.pos > eyeLevel person.pos then person else
    let moveDir = normalize (flatten (Model.direction person))
        strafeDir = transform (makeRotate (degrees -90) j) moveDir

        move = V3.scale (8.0 * toFloat directions.y) moveDir
        strafe = V3.scale (8.0 * toFloat directions.x) strafeDir
    in
        { person | velocity = adjustVelocity (move `add` strafe) }

adjustVelocity : Vec3 -> Vec3
adjustVelocity v =
    case toTuple v of
      (0,0,0) -> v
      _       -> V3.scale 20 (normalize v)

jump : EyeLevel -> Bool -> Model.Person -> Model.Person
jump eyeLevel isJumping person =
  if not isJumping || getY person.pos > eyeLevel person.pos then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x 2 v.z }

physics : EyeLevel -> Float -> Model.Person -> Model.Person
physics eyeLevel dt person =
    let pos = Terrain.bounds <| person.pos `add` V3.scale dt person.velocity
        p = toRecord pos
        e = eyeLevel pos

        pos' = if p.y < e
                    then vec3 p.x e p.z
                    else pos
    in
        { person | pos = pos' }

gravity : EyeLevel -> Float -> Model.Person -> Model.Person
gravity eyeLevel dt person =
  if getY person.pos <= eyeLevel person.pos then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x (v.y - (2*80) * dt) v.z }
