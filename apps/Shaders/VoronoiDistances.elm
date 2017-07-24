module Shaders.VoronoiDistances exposing (voronoiDistances)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (Vec4)
import WebGL exposing (..)


-- https://www.shadertoy.com/view/ldl3W8


voronoiDistances : Shader {} { u | iResolution : Vec3, iGlobalTime : Float, iHMD : Float } { elm_FragColor : Vec4, elm_FragCoord : Vec2 }
voronoiDistances =
    [glsl|

precision mediump float;
uniform vec3 iResolution;
uniform float iGlobalTime;
uniform float iHMD;

varying vec4 elm_FragColor;
varying vec2 elm_FragCoord;

const vec2 Scale = vec2(0.1469278, 0.2350845);
//const vec2 Scale = vec2(0.1469278, 0.2350845);
const vec2 ScaleIn = vec2(3, 2.5);
//const vec2 ScaleIn = vec2(2.5, 1.5);
const vec4 HmdWarpParam   = vec4(1, 0.22, 0.24, 0);

// Scales input texture coordinates for distortion.
vec2 HmdWarp(vec2 in01, vec2 LensCenter)
{
	vec2 theta = (in01 - LensCenter) * ScaleIn; // Scales to [-1, 1]
	float rSq = theta.x * theta.x + theta.y * theta.y;
	vec2 rvector = theta * (HmdWarpParam.x + HmdWarpParam.y * rSq +
		HmdWarpParam.z * rSq * rSq +
		HmdWarpParam.w * rSq * rSq * rSq);
	return LensCenter + Scale * rvector;
}

// Created by inigo quilez - iq/2013
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.


// I've not seen anybody out there computing correct cell interior distances for Voronoi
// patterns yet. That's why they cannot shade the cell interior correctly, and why you've
// never seen cell boundaries rendered correctly.

// However, here's how you do mathematically correct distances (note the equidistant and non
// degenerated grey isolines inside the cells) and hence edges (in yellow):

// http://www.iquilezles.org/www/articles/voronoilines/voronoilines.htm

vec2 hash2( vec2 p )
{
	// texture based white noise
	//return texture2D( iChannel0, (p+0.5)/256.0, -100.0 ).xy;

        // procedural white noise
	return fract(sin(vec2(dot(p,vec2(127.1,311.7)),dot(p,vec2(269.5,183.3))))*43758.5453);
}

vec3 voronoi( in vec2 x )
{
    vec2 n = floor(x);
    vec2 f = fract(x);

    //----------------------------------
    // first pass: regular voronoi
    //----------------------------------
	vec2 mg, mr;

    float md = 8.0;
    for( int j=-1; j<=1; j++ )
    for( int i=-1; i<=1; i++ )
    {
        vec2 g = vec2(float(i),float(j));
		vec2 o = hash2( n + g );
        o = 0.5 + 0.5*sin( iGlobalTime + 6.2831*o );
        vec2 r = g + o - f;
        float d = dot(r,r);

        if( d<md )
        {
            md = d;
            mr = r;
            mg = g;
        }
    }

    //----------------------------------
    // second pass: distance to borders
    //----------------------------------
    md = 8.0;
    for( int j=-2; j<=2; j++ )
    for( int i=-2; i<=2; i++ )
    {
        vec2 g = mg + vec2(float(i),float(j));
		vec2 o = hash2( n + g );
        o = 0.5 + 0.5*sin( iGlobalTime + 6.2831*o );
        vec2 r = g + o - f;

        if( dot(mr-r,mr-r)>0.00001 )
        md = min( md, dot( 0.5*(mr+r), normalize(r-mr) ) );
    }

    return vec3( md, mr );
}

void voronoiDistances(vec2 tc)
{
    //vec2 p = gl_FragCoord.xy/iResolution.xx;
    vec2 p = tc.xy;

    vec3 c = voronoi( 8.0*p );

	// isolines
    vec3 col = c.x*(0.5 + 0.5*sin(64.0*c.x))*vec3(1.0);
    // borders
    col = mix( vec3(1.0,0.6,0.0), col, smoothstep( 0.04, 0.07, c.x ) );
    // feature points
	float dd = length( c.yz );
	col = mix( vec3(1.0,0.6,0.1), col, smoothstep( 0.0, 0.12, dd) );
	col += vec3(1.0,0.6,0.1)*(1.0-smoothstep( 0.0, 0.04, dd));

	gl_FragColor = vec4(col,1.0);
}

void hmd( void )
{
    vec2 LensCenter = vec2(0.5, 0.5);
    vec2 ScreenCenter = vec2(0.5, 0.5);

    vec2 oTexCoord = gl_FragCoord.xy / iResolution.xy;

    vec2 tc = HmdWarp(oTexCoord, LensCenter);
    if (any(bvec2(clamp(tc,ScreenCenter-vec2(0.5,0.5), ScreenCenter+vec2(0.5,0.5)) - tc)))
    {
        gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
        return;
    }

    voronoiDistances(tc);
}

void main() {
    if (iHMD == 1.0)
        hmd();
    else
        voronoiDistances(elm_FragCoord);
}

|]
