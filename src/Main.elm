module Main (main) where

{-| This module drives a virtuul wurld

# Program entry
@docs main
-}

import Graphics.Element exposing (Element)
import Signal exposing (..)
import Time exposing (Time, fps)

import WebGL exposing (..)
import Math.Matrix4 exposing (..)
import Keyboard
import Mouse
import Window

import Model
import Update
import Display

{-
-- Pointer Lock information
port movement : Signal (Int,Int)
port isLocked : Signal Bool
-}

-- Set up 3D world
inputs : Signal Model.Inputs
inputs =
  let dt = map (\t -> t/500) (fps 60)
      dirKeys = merge Keyboard.arrows Keyboard.wasd
  in  merge (sampleOn dt <| map3 Model.TimeDelta Keyboard.space dirKeys dt)
            (map Model.Mouse movement)

{-| Mouse movement deltas -}
movement : Signal (Int, Int)
movement = map fst (foldp (\(x, y) (_, (px, py)) -> ((x-px, y-py), (x, y))) ((0,0),(0,0)) (Mouse.position))

person : Signal Model.Person
person = foldp Update.step Model.defaultPerson inputs

{-| The main function -}
main : Signal Element
main = world Display.crateEntities

world : ((Int,Int) -> Time -> Mat4 -> List Renderable) -> Signal Element
world entities =
  let t = foldp (+) 0 (fps 60)
  -- in  map5 Display.scene (constant entities) Window.dimensions t isLocked person
  in  map5 Display.scene (constant entities) Window.dimensions t (constant True) person

{-
-- Ability to request and exit. Click screen to request lock. Press escape to
-- give up the lock. This code can all be removed if you want to do this
-- differently.

port requestPointerLock : Signal ()
port requestPointerLock =
    dropWhen (lift2 (&&) Keyboard.shift isLocked) () Mouse.clicks

port exitPointerLock : Signal ()
port exitPointerLock =
    always () <~ keepIf (any (\x -> x == 27)) [] Keyboard.keysDown
-}
