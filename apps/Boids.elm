module Boids exposing (create)

import App exposing (..)
import Behavior.Boids as Boids exposing (..)
import Body exposing (..)
import Body.BFly exposing (bfly)
import Color exposing (white)
import FontAwesome
import Html exposing (Html)
import Html.Attributes as Html
import Math.RandomVector exposing (randomVec3)
import Math.Vector3 exposing (Vec3, add, vec3)
import Random
import Shaders.VoronoiDistances exposing (voronoiDistances)


type alias Boids =
    List (Boid (Visible {}))


type Msg
    = BoidsGenerated Boids


create : Int -> ( App, Cmd AppMsg )
create n =
    App.createUncontrolled (init n)
        { id = always "boids"
        , label = always "Boids"
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = framing
        , focus = focus
        , overlay = overlay
        , reposition = reposition
        }


randomBFly : Random.Generator (Visible (Oriented {}))
randomBFly =
    Random.map (bfly voronoiDistances) (Random.float 0.0 1.0)


randomBoid : Random.Generator (Boid (Visible {}))
randomBoid =
    Random.map3
        (newBoid 0.3 1.0)
        (Random.map (add (vec3 7 8 4)) (randomVec3 4.0))
        (randomVec3 1.0)
        randomBFly


randomBoids : Int -> Random.Generator Boids
randomBoids n =
    Random.list n randomBoid


init : Int -> ( Boids, Cmd Msg )
init n =
    ( [], Random.generate BoidsGenerated (randomBoids n) )


update : Msg -> Boids -> ( Boids, Cmd Msg )
update msg model =
    case msg of
        BoidsGenerated newBoids ->
            ( newBoids, Cmd.none )


animate : Ground -> Time -> Boids -> Boids
animate ground dt boids =
    moveBoids Boids.default ground dt boids


bodies : Boids -> Vec3 -> List Body
bodies boids pos =
    List.map toBody boids


reposition : Maybe AppPosition -> Boids -> Boids
reposition _ =
    identity


framing : PartyKey -> Boids -> Maybe Framing
framing _ boids =
    Maybe.map toFraming (List.head boids)


focus : Boids -> Maybe Focus
focus boids =
    Maybe.map orientedToFocus (List.head boids)


overlay : Boids -> Html msg
overlay _ =
    let
        textLeft =
            Html.style [ ( "text-align", "left" ) ]
    in
        Html.div []
            [ Html.h2 []
                [ Html.text "Boids" ]
            , Html.text "A model of natural flocking."
            , Html.div
                [ Html.style
                    [ ( "padding-left", "3em" )
                    , ( "padding-right", "3em" )
                    ]
                ]
                [ Html.p
                    [ textLeft ]
                    [ Html.text
                        ("The algorithm models the behavior of flocking animals (eg. birds) "
                            ++ "by simple rules which describe only the behavior of individuals."
                        )
                    ]
                , Html.p
                    [ textLeft ]
                    [ Html.text "Briefly, the rules are:" ]
                , Html.ol
                    [ Html.style
                        [ ( "width", "80%" )
                        , ( "height", "20%" )
                        , ( "margin", "auto" )
                        ]
                    ]
                    [ Html.li
                        [ textLeft ]
                        [ Html.text "Boids try to fly towards the centre of mass of neighbouring boids." ]
                    , Html.li
                        [ textLeft ]
                        [ Html.text "Boids try to keep a small distance away from other objects (including other boids)." ]
                    , Html.li
                        [ textLeft ]
                        [ Html.text "Boids try to match velocity with near boids." ]
                    ]
                , Html.p
                    [ textLeft ]
                    [ Html.text
                        "If you would like to learn more about how these rules are implemented see "
                    , Html.a
                        [ Html.href "http://www.kfish.org/boids/pseudocode.html"
                        , Html.target "_blank"
                        ]
                        [ Html.text "Conrad Parker's boids pseudocode." ]
                    ]
                , Html.p
                    [ textLeft ]
                    [ Html.text
                        "For a full explanation and an informative history see "
                    , Html.a
                        [ Html.href "http://www.red3d.com/cwr/boids/"
                        , Html.target "_blank"
                        ]
                        [ Html.text "Craig Reynolds' boids page." ]
                    ]
                ]
            , Html.p
                [ textLeft
                , Html.style [ ( "bottom", "0px" ) ]
                ]
                [ Html.a
                    [ Html.href "https://github.com/kfish/dreambuggy/blob/master/apps/Behavior/Boids.elm"
                    , Html.target "_blank"
                    ]
                    [ Html.text "View source on GitHub" ]
                ]
            ]
