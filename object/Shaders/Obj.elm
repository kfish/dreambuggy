module Shaders.Obj exposing (..)

import GLSLPasta
import GLSLPasta.Lighting as Lighting
import WebGL


{-| This shader uses Spherical Environment Mapping (SEM).
Here are some relevant links:

  - [very cool demo](https://www.clicktorelease.com/code/spherical-normal-mapping/#)
  - <https://www.clicktorelease.com/blog/creating-spherical-environment-mapping-shader>
  - <http://www.ozone3d.net/tutorials/glsl_texturing_p04.php>

-}
reflectionVert =
    GLSLPasta.combine
        [ Lighting.vertexReflection
        , Lighting.worldPosition
        ]
    |> WebGL.unsafeShader


reflectionFrag =
    GLSLPasta.combine
        [ Lighting.fragmentReflection
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader


{-| normal mapping according to:
<http://www.gamasutra.com/blogs/RobertBasler/20131122/205462/Three_Normal_Mapping_Techniques_Explained_For_the_Mathematically_Uninclined.php?print=1>
-}
normalVert =
    GLSLPasta.combine
        [ Lighting.vertexNormal
        , Lighting.worldPosition
        ]
        |> WebGL.unsafeShader


normalFrag =
    GLSLPasta.combine
        [ Lighting.fragmentNormal
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader


{-| same as the normal mapping shader, but without deforming normals.
-}
noNormalVert =
    GLSLPasta.combine
        [ Lighting.vertexNoNormal
        , Lighting.worldPosition
        ]
        |> WebGL.unsafeShader


noNormalFrag =
    GLSLPasta.combine
        [ Lighting.fragmentNoNormal
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader


{-| same as above, but without any textures.
-}
simpleVert =
    GLSLPasta.combine
        [ Lighting.vertexSimple
        , Lighting.worldPosition
        ]
        |> WebGL.unsafeShader


simpleFrag =
    GLSLPasta.combine
        [ Lighting.fragmentSimple
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader
