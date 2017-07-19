module Balls exposing (create)

import App exposing (..)
import Body exposing (..)
import Html exposing (Html)
import Html.Attributes as Html
import Math.RandomVector exposing (randomVec3)
import Math.Vector3 exposing (Vec3, add, vec3)
import Physics.Collisions exposing (collisions)
import Physics.Gravity exposing (gravity)
import Primitive.Sphere exposing (fogMountainsSphere)
import Random


type alias BBall a =
    Massive (Spherical (Moving a))


type alias Balls =
    List (BBall (Visible {}))


type Msg
    = BallsGenerated Balls


create : Int -> ( App, Cmd AppMsg )
create n =
    App.createUncontrolled (init n)
        { id = always "balls"
        , label = always "Balls"
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = framing
        , focus = focus
        , overlay = overlay
        , reposition = reposition
        }


newDrop : Vec3 -> Vec3 -> Oriented (Visible {}) -> BBall (Visible {})
newDrop pos vel thing0 =
    { radius = 1.0
    , mass = 1.0
    , velocity = vel
    , scale = thing0.scale
    , position = pos
    , orientation = thing0.orientation
    , appear = thing0.appear
    }


randomDrop : Random.Generator (BBall (Visible {}))
randomDrop =
    Random.map2
        (\pos vel -> newDrop pos vel fogMountainsSphere)
        (Random.map (add (vec3 0 30 0)) (randomVec3 4.0))
        (randomVec3 8.0)


randomBalls : Int -> Random.Generator Balls
randomBalls n =
    Random.list n randomDrop


init : Int -> ( Balls, Cmd Msg )
init n =
    ( [], Random.generate BallsGenerated (randomBalls n) )


update : Msg -> Balls -> ( Balls, Cmd Msg )
update msg balls =
    case msg of
        BallsGenerated newBalls ->
            ( newBalls, Cmd.none )


animate : Ground -> Time -> Balls -> Balls
animate ground dt balls =
    collisions dt (gravity ground dt balls)


bodies : Balls -> Vec3 -> List Body
bodies balls _ =
    List.map toBody balls


reposition : Maybe AppPosition -> Balls -> Balls
reposition _ =
    identity


framing : PartyKey -> Balls -> Maybe Framing
framing _ balls =
    Maybe.map toFraming (List.head balls)


focus : Balls -> Maybe Focus
focus balls =
    Nothing


overlay : Balls -> Html msg
overlay _ =
    let
        textLeft =
            Html.style [ ( "text-align", "left" ) ]
    in
        Html.div []
            [ Html.h2 []
                [ Html.text "Bouncing Balls" ]
            , Html.text "A model of elastic collisions"
            , Html.div
                [ Html.style
                    [ ( "padding-left", "3em" )
                    , ( "padding-right", "3em" )
                    ]
                ]
                [ Html.p
                    [ textLeft ]
                    [ Html.text
                        "This models elastic collision between spheres of varying radius and mass."
                    ]
                , Html.p
                    [ textLeft ]
                    [ Html.text
                        ("An elastic collision is an encounter between two bodies "
                            ++ "in which the total kinetic energy of the two bodies after "
                            ++ "the encounter is equal to their total kinetic energy before "
                            ++ "the encounter. Perfectly elastic collisions occur only if "
                            ++ "there is no net conversion of kinetic energy into other forms "
                            ++ "(such as heat or noise) and therefore they do not normally "
                            ++ "occur in reality. "
                        )
                    ]
                , Html.p
                    [ textLeft ]
                    [ Html.text "("
                    , Html.a
                        [ Html.href "https://en.wikipedia.org/wiki/Elastic_collision"
                        , Html.target "_blank"
                        ]
                        [ Html.text "Elastic collision" ]
                    , Html.text ", WIkipedia)"
                    ]
                ]
            , Html.p
                [ textLeft
                , Html.style [ ( "bottom", "0px" ) ]
                ]
                [ Html.a
                    [ Html.href "https://github.com/kfish/dreambuggy/blob/master/engine/Physics/Collisions.elm"
                    , Html.target "_blank"
                    ]
                    [ Html.text "View source on GitHub" ]
                ]
            ]
