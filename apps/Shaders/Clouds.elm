module Shaders.Clouds exposing (clouds)

import GLSLPasta
import GLSLPasta.Core exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (Vec4, vec4)
import Shaders.HMD exposing (hmdTemplate)
import WebGL exposing (..)


-- https://www.shadertoy.com/view/XslGRr

fragment_clouds : GLSLPasta.Component
fragment_clouds =
    { empty
        | id = "fragment_clouds"
        , provides = [ "gl_FragColor" ]
        , globals =
            [ Uniform "vec3" "iResolution"
            , Uniform "float" "iGlobalTime"
            , Varying "vec4" "elm_FragColor"
            , Varying "vec2" "elm_FragCoord"
            ]
        , functions =
            [ """
// Created by inigo quilez - iq/2013
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

// hash based 3d value noise
float hash( float n )
{
    return fract(sin(n)*43758.5453);
}
float noise( in vec3 x )
{
    vec3 p = floor(x);
    vec3 f = fract(x);

    f = f*f*(3.0-2.0*f);
    float n = p.x + p.y*57.0 + 113.0*p.z;
    return mix(mix(mix( hash(n+  0.0), hash(n+  1.0),f.x),
                   mix( hash(n+ 57.0), hash(n+ 58.0),f.x),f.y),
               mix(mix( hash(n+113.0), hash(n+114.0),f.x),
                   mix( hash(n+170.0), hash(n+171.0),f.x),f.y),f.z);
}

vec4 map( in vec3 p )
{
    float d = 0.2 - p.y;

    vec3 q = p - vec3(1.0,0.1,0.0)*iGlobalTime;
    float f;
    f  = 0.5000*noise( q ); q = q*2.02;
    f += 0.2500*noise( q ); q = q*2.03;
    f += 0.1250*noise( q ); q = q*2.01;
    f += 0.0625*noise( q );

    d += 3.0 * f;

    d = clamp( d, 0.0, 1.0 );

    vec4 res = vec4( d );

    res.xyz = mix( 1.15*vec3(1.0,0.95,0.8), vec3(0.7,0.7,0.7), res.x );

    return res;
}


vec3 sundir = vec3(-1.0,0.0,0.0);


vec4 raymarch( in vec3 ro, in vec3 rd )
{
    vec4 sum = vec4(0, 0, 0, 0);

    float t = 0.0;
    for(int i=0; i<64; i++)
    {
        if( sum.a > 0.99 ) continue;

        vec3 pos = ro + t*rd;
        vec4 col = map( pos );

        float dif =  clamp((col.w - map(pos+0.3*sundir).w)/0.6, 0.0, 1.0 );

                vec3 lin = vec3(0.65,0.68,0.7)*1.35 + 0.45*vec3(0.7, 0.5, 0.3)*dif;
        col.xyz *= lin;

        col.a *= 0.35;
        col.rgb *= col.a;

        sum = sum + col*(1.0 - sum.a);

        t += max(0.1, 0.025*t);
    }

    sum.xyz /= (0.001+sum.w);

    return clamp( sum, 0.0, 1.0 );
}

vec4 clouds(vec2 tc)
{
    // vec2 q = gl_FragCoord.xy / iResolution.xy;
    vec2 q = tc.xy;
    vec2 p = -1.0 + 2.0*q;
    //p.x *= iResolution.x/ iResolution.y;
    p.x *= 1.5;
    //vec2 mo = -1.0 + 2.0*iMouse.xy / iResolution.xy;

    // camera
    //vec3 ro = 4.0*normalize(vec3(cos(2.75-3.0*mo.x), 0.7+(mo.y+1.0), sin(2.75-3.0*mo.x)));
    vec3 ro = 4.0*normalize(vec3(cos(2.75-1.3), 0.7, sin(2.75-2.0)));
    vec3 ta = vec3(0.0, 1.0, 0.0);
    vec3 ww = normalize( ta - ro);
    vec3 uu = normalize(cross( vec3(0.0,1.0,0.0), ww ));
    vec3 vv = normalize(cross(ww,uu));
    vec3 rd = normalize( p.x*uu + p.y*vv + 1.5*ww );

    vec4 res = raymarch( ro, rd );

    float sun = clamp( dot(sundir,rd), 0.0, 1.0 );
    vec3 col = vec3(0.6,0.71,0.75) - rd.y*0.2*vec3(1.0,0.5,1.0) + 0.15*0.5;
    col += 0.2*vec3(1.0,.6,0.1)*pow( sun, 8.0 );
    col *= 0.95;
    col = mix( col, res.xyz, res.w );
    col += 0.1*vec3(1.0,0.4,0.2)*pow( sun, 3.0 );

    return vec4( col, 1.0 );
}
"""
            ]
        , splices =
            [ """
            gl_FragColor = clouds(fragCoord);
"""
            ]
    }


clouds : Shader {} { u | iResolution : Vec3, iGlobalTime : Float, iHMD : Float } { elm_FragColor : Vec4, elm_FragCoord : Vec2, clipPosition : Vec4 }
clouds =
    GLSLPasta.combineUsingTemplate hmdTemplate "clouds"
        [ fragment_clouds
        , Lighting.lightenDistance
        ]
    |> WebGL.unsafeShader

