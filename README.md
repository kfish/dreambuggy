# Dreambuggy

Drive dreambuggy on mountains, snow and sand. Fly dreambird.

Dreambuggy is the original demo for [Here4 (github.com)](https://www.github.com/here4/here4), a 3D app framework.
Here4 lets you create worlds connected by portals, and code up apps for things like
vehicles, portals, interactive objects and games.

## Play in your browser

 * [Live demo (kfish.github.io/dreambuggy)][demo]

[demo]: http://kfish.github.io/dreambuggy/

## Demo videos

_Subscribe to [my YouTube channel](https://www.youtube.com/channel/UCyjxiIzul2SBjRTEGi9CZ3A "Conrad Parker's YouTube channel") for more!_

[![Dreambuggy demo video](http://img.youtube.com/vi/RDFuTzPQ3Sc/0.jpg)](http://www.youtube.com/watch?v=RDFuTzPQ3Sc "Play on YouTube")
[![Dreambird demo video](http://img.youtube.com/vi/kW4eseUG9b4/0.jpg)](http://www.youtube.com/watch?v=kW4eseUG9b4 "Play on YouTube")

## Controls

Plug in a gamepad, or use mouse and keyboard arrows or WASD keys to move.
Supports up to 2 USB gamepads.

Press the spacebar or gamepad button Y to enter/exit vehicles and portals.

|    | :computer: Keyboard | :video_game: Gamepad |
|---|---|---|
| Enter/exit vehicles and portals | Space | Y |
| Show app info / movement controls | i | Guide (eg. Xbox Logo) |
| Change camera | c, C | Right, left bumpers |
| Move camera | h,j,k,l | DPad left,down,up,right |

## Build Locally

This package depends on here4/here4,
which uses experimental Native code and is not yet
whiteliested for inclusion in the Elm package archive.
You will need to use an unofficial installer like
[elm-install](https://github.com/gdotdesign/elm-github-install)

Clone this repository and run a local HTTP file server,
such as the included ./server.py:

```bash
git clone https://github.com/kfish/dreambuggy.git
cd dreambuggy
elm-install
make
./server.py
```

And then open [http://localhost:8000/index.html](http://localhost:8000/index.html) to see it in action!

## Credits

Built with [Elm](http://elm-lang.org/).

This demo includes the following shaders:

  * [Fire](https://www.shadertoy.com/view/Xsl3zN) by 301
