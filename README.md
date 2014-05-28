# Shadertoy for Elm

[Shadertoy](http://shadertoy.com/) is ... elite.

You write your shaders in GLSL, and game logic and physics in Elm.

The Elm code here is forked from 
Evan Czaplicki's [first-person-elm](https://github.com/evancz/first-person-elm) demo.

Make sure you have the latest version of Chrome or Firefox and then click the
following image to try out the **[live demo][demo]**:

[![Live Demo](resources/ScreenShot.png)][demo]

[demo]: http://kfish.github.io/elm-shadertoy/

## Importing Shadertoy fragment shaders

Shaders in elm are included verbatim in a [glsl| ... ] block.

Use the following types and preamble to define a fragment shader named `foo`:

```glsl
foo : Shader {} { u | iResolution:Vec3, iGlobalTime:Float } { elm_FragCoord:Vec2 }
foo = [glsl|

precision mediump float;
uniform vec3 iResolution;
uniform float iGlobalTime;

varying vec2 elm_FragCoord;

<<<SHADER CODE GOES HERE>>>

|]
```

### Inputs

Shadertoy defines various inputs to fragment shaders. elm-shadertoy provides
compatibility for the following:


```glsl
uniform vec3      iResolution;           // viewport resolution (in pixels)
uniform float     iGlobalTime;           // shader playback time in seconds
```

elm-shadertoy additionally defines 'elm_FragCoord'.

```glsl
varying vec2      elm_FragCoord;         // texture-space fragment coordinate
```

Replace all occurrences of `gl_FragCoord.xy / iResolution.xy` with `elm_FragCoord.xy`. This
ensures that pixels are calculated according to their location on 3D surface, rather than
their location on your 2D screen.


### What is NOT (YET) supported

The following Shadertoy inputs are not yet supported by elm-shadertoy:

```
uniform float     iChannelTime[4];       // channel playback time (in seconds)
uniform vec3      iChannelResolution[4]; // channel resolution (in pixels)
uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
uniform samplerXX iChannel0..3;          // input channel. XX=2D/Cube
uniform vec4      iDate;                 // (year, month, day, time in seconds)
```

These are tracked in issues in this project (elm-shadertoy):
  1. [Support Shadertoy channel inputs](https://github.com/kfish/elm-shadertoy/issues/1)
  2. [Support Shadertoy mouse input](https://github.com/kfish/elm-shadertoy/issues/2)
  3. [Support Shadertoy date input](https://github.com/kfish/elm-shadertoy/issues/3)

Additionally, there is an issue in the Haskell language-glsl package regarding the
[GLSL preprocessor: support for #define, #ifdef etc.](https://github.com/noteed/language-glsl/issues/4),
so you need to manually preprocess (replace constants by variables,
use comments to select behavior instead of #ifdef).

## Build Locally

After installing [the Elm Platform](https://github.com/elm-lang/elm-platform),
run the following sequence of commands:

```bash
git clone https://github.com/evancz/first-person-elm.git
cd first-person-elm
elm-get install
elm --make --only-js --src-dir=src Main.elm
elm-server
```

And then open [http://localhost:8000](http://localhost:8000) to see it in action!
