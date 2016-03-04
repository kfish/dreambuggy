module Shaders.NoiseVertex (NoiseVertex, noiseVertex) where

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Math.Matrix4 exposing (Mat4)
import WebGL exposing (..)

type alias NoiseVertex = { pos:Vec3, color:Vec4, coord:Vec3, textureScale:Float, timeScale:Float, smoothing:Float }

noiseVertex : Shader NoiseVertex { u | view:Mat4 } { elm_FragColor:Vec4, elm_FragCoord:Vec2, iTextureScale:Float, iTimeScale:Float, iSmoothing:Float }
noiseVertex = [glsl|

attribute vec3 pos;
attribute vec4 color;
attribute vec3 coord;
attribute float textureScale;
attribute float timeScale;
attribute float smoothing;
uniform mat4 view;
varying vec4 elm_FragColor;
varying vec2 elm_FragCoord;
varying float iTextureScale;
varying float iTimeScale;
varying float iSmoothing;
void main () {
  gl_Position = view * vec4(pos, 1.0);
  elm_FragColor = color;
  elm_FragCoord = coord.xy;
  iTextureScale = textureScale;
  iTimeScale = timeScale;
  iSmoothing = smoothing;
}

|]