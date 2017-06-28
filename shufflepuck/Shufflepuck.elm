module Shufflepuck exposing (create, default)

import Html exposing (Html)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Task exposing (Task)
import Time exposing (Time)
import Tuple exposing (first)
import WebGL.Texture as Texture exposing (Texture, Error)
import App exposing (App, AppMsg, Focus, appToFocus)
import Body exposing (..)
import Camera exposing (..)
import Camera.Util as Camera
import Control exposing (CtrlMsg)
import Dispatch exposing (..)
import Ground exposing (Ground)
import Model exposing (Inputs)
import Orientation exposing (Orientation)
import Primitive.Cube exposing (cloudsCube, textureCube)
import Primitive.Cylinder exposing (cloudsCylinder, textureCylinder)

import Bounding exposing (Bounding, bounce, bump)
import Bounding.Box exposing (Box, boundingBox)
import Physics.Collisions exposing (collisions)

type alias Attributes =
    { label : String
    , position : Vec3
    , scale : Vec3
    , orientation : Orientation
    , tableTexture : String
    , tableWidth : Float
    , tableLength : Float
    , tableThickness : Float
    , tableHeight : Float
    , goalWidth : Float
    , puckTexture : String
    , puckMass : Float
    , puckRadius : Float
    , puckThickness : Float
    , puckHover : Float
    , puckMaxSpeed : Float
    , paddleTexture : String
    , paddleMass : Float
    , paddleRadius : Float
    , paddleThickness : Float
    , paddleHover : Float
    }

default : Attributes
default =
    { label = "Shufflepuck"
    , position = vec3 0 0 0
    , scale = vec3 1 1 1
    , orientation = Orientation.initial
    , tableTexture = "textures/decursio_top.jpg"
    , tableWidth = 3
    , tableLength = 7
    , tableHeight = 3.0 -- 0.9
    , tableThickness = 0.2
    , goalWidth = 0.8
    , puckTexture = "textures/mp_diff_orange.png"
    , puckMass = 5.0
    , puckRadius = 0.12
    , puckThickness = 0.03
    , puckHover = 0.3
    , puckMaxSpeed = 0.7
    , paddleTexture = "textures/ring_bottoms.jpg"
    , paddleMass = 10.0
    , paddleRadius = 0.18
    , paddleThickness = 0.07
    , paddleHover = 0.3
    }

type alias Model =
    { attributes : Attributes
    , table : Body
    , puck : Massive (Spherical (Moving Body))
    , paddle1 : Massive (Spherical (Moving Body))
    , paddle2 : Massive (Spherical (Moving Body))
    , bounds : Bounding Box
    , score1 : Int
    , score2 : Int
    }


type Msg
    = TableTextureLoaded (Result Error Texture)
    | PuckTextureLoaded (Result Error Texture)
    | PaddleTextureLoaded (Result Error Texture)


create : Attributes -> ( App, Cmd AppMsg )
create attributes =
    App.create (init attributes)
        { label = label
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = framing
        , focus = focus
        , overlay = overlay
        }

label : Model -> String
label model = model.attributes.label ++ " (" ++ toString model.score1 ++ " - " ++ toString model.score2 ++ ")"

-- | Move the entire table, maintaining the relative positions of the paddles and puck
reposition : Vec3 -> Model -> Model
reposition pos0 model =
    let
        a = model.attributes

        puckRelPos = V3.sub model.puck.position a.position
        paddle1RelPos = V3.sub model.paddle1.position a.position
        paddle2RelPos = V3.sub model.paddle2.position a.position

        bounds = model.bounds
        box = bounds.model
        boundsRelPos = V3.sub box.position a.position

        setPos pos body = { body | position = pos }
    in
        { model | attributes = { a | position = pos0 }
                , table = setPos pos0 model.table
                , puck = setPos (V3.add pos0 puckRelPos) model.puck
                , paddle1 = setPos (V3.add pos0 paddle1RelPos) model.paddle1
                , paddle2 = setPos (V3.add pos0 paddle2RelPos) model.paddle2
                , bounds = { bounds | model = setPos (V3.add pos0 boundsRelPos) box }
        } 


init : Attributes -> ( Model, Cmd (CtrlMsg Msg) )
init a =
    let
        puckY = a.puckHover -- + (a.tableThickness + a.puckThickness) / 2.0
        paddleY = a.paddleHover -- + (a.tableThickness + a.paddleThickness) / 2.0
        paddleZ = a.tableLength / 4.0

        boundHeight = a.tableThickness/2 + max (a.puckHover + a.puckThickness) (a.paddleThickness + a.paddleHover)
    in
        ( reposition a.position
            { attributes = { a | position = vec3 0 0 0 } -- First set up the table at 0 0 0, then reposition it
            , table =
                { anchor = AnchorGround
                , scale = vec3 a.tableWidth a.tableThickness a.tableLength
                , position = a.position
                , orientation = Orientation.initial
                , appear = cloudsCube
                }
            , puck =
                  { anchor = AnchorGround
                  , scale = vec3 a.puckRadius (a.puckThickness/2.0) a.puckRadius
                  , position = vec3 0 puckY 0
                  , orientation = Orientation.initial
                  , radius = a.puckRadius
                  , mass = a.puckMass
                  , appear = cloudsCylinder
                  , velocity = vec3 0 0 0
                  }
            , paddle1 =
                  { anchor = AnchorGround
                  , scale = vec3 a.paddleRadius (a.paddleThickness/2.0) a.paddleRadius
                  , position = vec3 0 paddleY -paddleZ
                  , orientation = Orientation.initial
                  , radius = a.paddleRadius
                  , mass = a.paddleMass
                  , appear = cloudsCylinder
                  , velocity = vec3 0 0 0
                  }
            , paddle2 =
                  { anchor = AnchorGround
                  , scale = vec3 a.paddleRadius (a.paddleThickness/2.0) a.paddleRadius
                  , position = vec3 0 paddleY paddleZ
                  , orientation = Orientation.initial
                  , radius = a.paddleRadius
                  , mass = a.paddleMass
                  , appear = cloudsCylinder
                  , velocity = vec3 0 0 0
                  }
            , bounds = boundingBox { position = vec3 (-a.tableWidth/2.0) (a.tableThickness/2.0) (-a.tableLength/2.0)
                                   , dimensions = vec3 a.tableWidth boundHeight a.tableLength
                                   }
            , score1 = 0
            , score2 = 0
            }
        , Cmd.batch
              [ Texture.load a.tableTexture
                  |> Task.attempt (Self << TableTextureLoaded)
              , Texture.load a.puckTexture
                  |> Task.attempt (Self << PuckTextureLoaded)
              , Texture.load a.paddleTexture
                  |> Task.attempt (Self << PaddleTextureLoaded)
              ]
        )


update :
    CtrlMsg Msg
    -> Model
    -> ( Model, Cmd (CtrlMsg Msg) )
update msg model =
    case msg of
        Self (TableTextureLoaded textureResult) ->
            case textureResult of
                Ok texture ->
                    let
                        t = model.table
                        table = { t | appear = textureCube texture }
                    in
                        ( { model | table = table }, Cmd.none )

                Err msg ->
                    -- ( { model | message = "Error loading texture" }, Cmd.none )
                    ( model, Cmd.none )

        Self (PuckTextureLoaded textureResult) ->
            case textureResult of
                Ok texture ->
                    let
                        p = model.puck
                        puck = { p | appear = textureCylinder texture }
                    in
                        ( { model | puck = puck }, Cmd.none )

                Err msg ->
                    -- ( { model | message = "Error loading texture" }, Cmd.none )
                    ( model, Cmd.none )

        Self (PaddleTextureLoaded textureResult) ->
            case textureResult of
                Ok texture ->
                    let
                        p1 = model.paddle1
                        paddle1 = { p1 | appear = textureCylinder texture }

                        p2 = model.paddle2
                        paddle2 = { p2 | appear = textureCylinder texture }
                    in
                        ( { model | paddle1 = paddle1
                                  , paddle2 = paddle2
                          }
                        , Cmd.none
                        )

                Err msg ->
                    -- ( { model | message = "Error loading texture" }, Cmd.none )
                    ( model, Cmd.none )

        Ctrl (Control.Move dp) ->
            ( model, Cmd.none )

        Ctrl (Control.Drive ground inputs) ->
            ( movePaddle inputs model, Cmd.none )

        Effect _ ->
            ( model, Cmd.none )


movePaddle : Inputs -> Model -> Model
movePaddle inputs model =
    let
        dx = -2.0 * (inputs.x + inputs.mx)
        dy = 2.0 * (inputs.y + inputs.my)

        p = model.paddle1

        paddle = { p | velocity = vec3 dx 0 dy }
    in
        collide inputs.dt { model | paddle1 = paddle }


collide : Time -> Model -> Model
collide dt model =
    let
        a = model.attributes
        box = model.bounds.model
        (px, py, pz) = V3.toTuple <| V3.sub model.puck.position box.position
        (rx, ry, rz) = V3.toTuple <| V3.sub (V3.add model.puck.position (V3.scale dt model.puck.velocity)) box.position

        recenter p = { p | position = V3.add (vec3 0 a.puckHover 0) a.position
                         , velocity = vec3 0 0 0
                     }

        z1 = V3.getZ box.dimensions - a.puckRadius
        over1 = (rz >= z1)
        x1 = px + (rx - px) * (z1- pz) / (rz - pz)
        goal1 = over1 && abs (x1 - a.tableWidth/2.0) < (a.goalWidth/2.0)

        z2 = a.puckRadius
        over2 = (rz <= z2)
        x2 = px + (rx - px) * (z2- pz) / (rz - pz)
        goal2 = over2 && abs (x2 - a.tableWidth/2.0) < (a.goalWidth/2.0)

        (puck, paddle1, paddle2, score1, score2) =
            if goal1 then
                (recenter model.puck, model.paddle1, model.paddle2, model.score1+1, model.score2)
            else if goal2 then
                (recenter model.puck, model.paddle1, model.paddle2, model.score1, model.score2+1)
            else
                let
                    puck_z = V3.getZ model.puck.position 
                    puck_0 = bounce model.bounds dt model.puck
                    paddle1_0 = bump model.bounds dt model.paddle1
                    paddle2_0 = bump model.bounds dt model.paddle2
                    bodies = collisions dt [ puck_0, paddle1_0, paddle2_0 ]
                    (puck, paddle1, paddle2) = case bodies of
                        [b1, b2, b3] -> (b1, b2, b3)
                        _ -> (model.puck, model.paddle1, model.paddle2)
                    puck_1 = { puck | velocity = v3_clamp model.attributes.puckMaxSpeed puck.velocity }
                in
                    (puck_1, paddle1, paddle2, model.score1, model.score2)
    in
        { model | puck = puck
                , paddle1 = paddle1
                , paddle2 = paddle2
                , score1 = score1
                , score2 = score2
        }


v3_clamp : Float -> Vec3 -> Vec3
v3_clamp len v =
    if V3.length v <= len then
        v
    else
        V3.scale len (V3.normalize v)


animate : Ground -> Time -> Model -> Model
animate ground dt model =
    let
        setElevation pos = V3.setY (model.attributes.tableHeight + ground.elevation pos) pos
    in
        reposition (setElevation model.attributes.position) (collide dt model)


bodies : Model -> List Body
bodies model =
    model.table :: List.map toBody [ model.puck, model.paddle1, model.paddle2 ]

framing : Model -> Maybe Framing
framing model =
    let
        target =
            { position = model.paddle1.position
            , orientation = model.attributes.orientation
            , velocity = vec3 0 0 0
            }
    in
        Just { target = target }
    

focus : Model -> Maybe Focus
focus model =
    Just <| appToFocus model.table

overlay : Model -> Html msg
overlay _ =
    Html.div []
        [ Html.h2 []
              [ Html.text "Shufflepuck" ]
        , Html.text "An air hockey game."
        , Html.br [] []
        , Html.hr [] []
        ]
