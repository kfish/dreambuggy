module Shufflepuck exposing (create, default)

import App exposing (..)
import App.Control exposing (..)
import Body exposing (..)
import Bounding exposing (Bounding, bounce, bump)
import Bounding.Box exposing (Box, boundingBox)
import Camera
import Html exposing (Html)
import Dispatch exposing (..)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Model exposing (Inputs)
import Orientation exposing (Orientation)
import Primitive.Cube exposing (cloudsCube, textureCube)
import Primitive.Cylinder exposing (cloudsCylinder, textureCylinder)
import Physics.Collisions exposing (collisions)
import Task exposing (Task)
import Tuple exposing (first)
import WebGL.Texture as Texture exposing (Texture, Error)


type alias Attributes =
    { id : String
    , label : String
    , position : Vec3
    , scale : Vec3
    , orientation : Orientation
    , tableTexture : String
    , tableWidth : Float
    , tableLength : Float
    , tableThickness : Float
    , tableHeight : Float
    , goalWidth : Float
    , hover : Float
    , puckTexture : String
    , puckMass : Float
    , puckRadius : Float
    , puckThickness : Float
    , puckMaxSpeed : Float
    , paddleTexture : String
    , paddleMass : Float
    , paddleRadius : Float
    , paddleThickness : Float
    }


default : Attributes
default =
    let
        golden =
            (1 + sqrt 5) / 2.0

        width =
            2.0

        length =
            width * golden
    in
        { id = "shufflepuck"
        , label = "Shufflepuck"
        , position = vec3 0 0 0
        , scale = vec3 1 1 1
        , orientation = Orientation.initial
        , tableTexture = "textures/decursio_top.jpg"
        , tableWidth = width
        , tableLength = length
        , tableHeight = 0.9
        , tableThickness = 0.2
        , goalWidth = width * 0.2
        , hover = 0.3
        , puckTexture = "textures/mp_diff_orange.png"
        , puckMass = 5.0
        , puckRadius = width * 0.03
        , puckThickness = 0.03
        , puckMaxSpeed = width * 0.25
        , paddleTexture = "textures/ring_bottoms.jpg"
        , paddleMass = 10.0
        , paddleRadius = width * 0.045
        , paddleThickness = 0.07
        }


type alias Model =
    { attributes : Attributes
    , table : Body
    , puck : Massive (Spherical (Moving Body))
    , paddle1 : Massive (Spherical (Moving Body))
    , paddle2 : Massive (Spherical (Moving Body))
    , puckBounds : Bounding Box
    , paddle1Bounds : Bounding Box
    , paddle2Bounds : Bounding Box
    , score1 : Int
    , score2 : Int
    , robotPaddle2 : RobotPaddle
    }


type alias RobotPaddle =
    { elapsedTime : Time
    }


initRobotPaddle : RobotPaddle
initRobotPaddle =
    { elapsedTime = 0
    }


updateRobotPaddle : Time -> RobotPaddle -> RobotPaddle
updateRobotPaddle dt rp =
    { elapsedTime = rp.elapsedTime + dt }


applyRobotPaddle : RobotPaddle -> Moving a -> Massive (Spherical (Moving Body)) -> Massive (Spherical (Moving Body))
applyRobotPaddle robot puck paddle =
    let
        t =
            robot.elapsedTime

        toPuck =
            V3.sub puck.position paddle.position
                |> V3.normalize
                |> V3.scale 0.2

        dx =
            0.1 * cos (20 * t) + 0.3 * sin (5.0 * t)

        dy =
            0.2 * sin (15 * t) + 0.15 * cos (10.0 * t)
    in
        { paddle | velocity = V3.add toPuck (vec3 dx 0 dy) }


type Msg
    = TableTextureLoaded (Result Error Texture)
    | PuckTextureLoaded (Result Error Texture)
    | PaddleTextureLoaded (Result Error Texture)


create : Attributes -> ( App, Cmd AppMsg )
create attributes =
    App.create (init attributes)
        { id = always attributes.id
        , label = label
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = framing
        , focus = focus
        , overlay = overlay
        , reposition = reposition
        }


label : Model -> String
label model =
    model.attributes.label ++ " (" ++ toString model.score1 ++ " - " ++ toString model.score2 ++ ")"



-- | TODO: Handle orientation


reposition : Maybe AppPosition -> Model -> Model
reposition mPos model =
    case mPos of
        Just pos ->
            setPosition pos.position model

        Nothing ->
            model



-- | Move the entire table, maintaining the relative positions of the paddles and puck


setPosition : Vec3 -> Model -> Model
setPosition pos0 model =
    let
        a =
            model.attributes

        puckRelPos =
            V3.sub model.puck.position a.position

        paddle1RelPos =
            V3.sub model.paddle1.position a.position

        paddle2RelPos =
            V3.sub model.paddle2.position a.position

        bounds =
            model.puckBounds

        box =
            bounds.model

        boundsRelPos =
            V3.sub box.position a.position

        updateBounds b =
            let
                box =
                    b.model

                relPos =
                    V3.sub box.position a.position
            in
                { b | model = setPos (V3.add pos0 relPos) box }

        setPos pos body =
            { body | position = pos }
    in
        { model
            | attributes = { a | position = pos0 }
            , table = setPos pos0 model.table
            , puck = setPos (V3.add pos0 puckRelPos) model.puck
            , paddle1 = setPos (V3.add pos0 paddle1RelPos) model.paddle1
            , paddle2 = setPos (V3.add pos0 paddle2RelPos) model.paddle2
            , puckBounds = updateBounds model.puckBounds
            , paddle1Bounds = updateBounds model.paddle1Bounds
            , paddle2Bounds = updateBounds model.paddle2Bounds
        }


init : Attributes -> ( Model, Cmd (CtrlMsg Msg) )
init a =
    let
        puckY =
            a.hover

        paddleY =
            a.hover

        paddleZ =
            a.tableLength / 4.0

        boundHeight =
            a.tableThickness / 2 + max (a.hover + a.puckThickness) (a.paddleThickness + a.hover)

        dimensions =
            vec3 a.tableWidth boundHeight a.tableLength

        box =
            { position = vec3 (-a.tableWidth / 2.0) a.tableThickness (-a.tableLength / 2.0)
            , dimensions = dimensions
            }

        halfZ =
            V3.getZ dimensions / 2.0

        halfDim =
            V3.setZ halfZ dimensions

        box1 =
            { box | dimensions = halfDim }

        bounds =
            boundingBox box

        bounds1 =
            boundingBox box1

        pos =
            box.position

        pos2 =
            V3.setZ (V3.getZ pos + halfZ) pos

        box2 =
            { box
                | position = pos2
                , dimensions = halfDim
            }

        bounds2 =
            boundingBox box2
    in
        ( setPosition a.position
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
                , scale = vec3 a.puckRadius (a.puckThickness / 2.0) a.puckRadius
                , position = vec3 0 puckY (-0.4 * a.tableLength / 2.0)
                , orientation = Orientation.initial
                , radius = a.puckRadius
                , mass = a.puckMass
                , appear = cloudsCylinder
                , velocity = vec3 0 0 0
                }
            , paddle1 =
                { anchor = AnchorGround
                , scale = vec3 a.paddleRadius (a.paddleThickness / 2.0) a.paddleRadius
                , position = vec3 0 paddleY -paddleZ
                , orientation = Orientation.initial
                , radius = a.paddleRadius
                , mass = a.paddleMass
                , appear = cloudsCylinder
                , velocity = vec3 0 0 0
                }
            , paddle2 =
                { anchor = AnchorGround
                , scale = vec3 a.paddleRadius (a.paddleThickness / 2.0) a.paddleRadius
                , position = vec3 0 paddleY paddleZ
                , orientation = Orientation.initial
                , radius = a.paddleRadius
                , mass = a.paddleMass
                , appear = cloudsCylinder
                , velocity = vec3 0 0 0
                }
            , puckBounds = bounds
            , paddle1Bounds = bounds1
            , paddle2Bounds = bounds2
            , score1 = 0
            , score2 = 0
            , robotPaddle2 = initRobotPaddle
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
                        t =
                            model.table

                        table =
                            { t | appear = textureCube texture }
                    in
                        ( { model | table = table }, Cmd.none )

                Err msg ->
                    -- ( { model | message = "Error loading texture" }, Cmd.none )
                    ( model, Cmd.none )

        Self (PuckTextureLoaded textureResult) ->
            case textureResult of
                Ok texture ->
                    let
                        p =
                            model.puck

                        puck =
                            { p | appear = textureCylinder texture }
                    in
                        ( { model | puck = puck }, Cmd.none )

                Err msg ->
                    -- ( { model | message = "Error loading texture" }, Cmd.none )
                    ( model, Cmd.none )

        Self (PaddleTextureLoaded textureResult) ->
            case textureResult of
                Ok texture ->
                    let
                        p1 =
                            model.paddle1

                        paddle1 =
                            { p1 | appear = textureCylinder texture }

                        p2 =
                            model.paddle2

                        paddle2 =
                            { p2 | appear = textureCylinder texture }
                    in
                        ( { model
                            | paddle1 = paddle1
                            , paddle2 = paddle2
                          }
                        , Cmd.none
                        )

                Err msg ->
                    -- ( { model | message = "Error loading texture" }, Cmd.none )
                    ( model, Cmd.none )

        Ctrl (Move dp) ->
            ( model, Cmd.none )

        Ctrl (Drive ground inputs) ->
            ( movePaddle inputs model, Cmd.none )

        _ ->
            ( model, Cmd.none )


movePaddle : Inputs -> Model -> Model
movePaddle inputs model =
    let
        dx =
            -2.0 * (inputs.x + inputs.mx)

        dy =
            2.0 * (inputs.y + inputs.my)

        p =
            model.paddle1

        paddle =
            { p | velocity = vec3 dx 0 dy }
    in
        collide inputs.dt { model | paddle1 = paddle }


collide : Time -> Model -> Model
collide dt model =
    let
        a =
            model.attributes

        box =
            model.puckBounds.model

        ( px, py, pz ) =
            V3.toTuple <| V3.sub model.puck.position box.position

        ( rx, ry, rz ) =
            V3.toTuple <| V3.sub (V3.add model.puck.position (V3.scale dt model.puck.velocity)) box.position

        recenter offset p =
            { p
                | position = V3.add (vec3 0 a.hover (offset * a.tableLength / 2.0)) a.position
                , velocity = vec3 0 0 0
            }

        z1 =
            V3.getZ box.dimensions - a.puckRadius

        over1 =
            (rz >= z1)

        x1 =
            px + (rx - px) * (z1 - pz) / (rz - pz)

        goal1 =
            over1 && abs (x1 - a.tableWidth / 2.0) < (a.goalWidth / 2.0)

        z2 =
            a.puckRadius

        over2 =
            (rz <= z2)

        x2 =
            px + (rx - px) * (z2 - pz) / (rz - pz)

        goal2 =
            over2 && abs (x2 - a.tableWidth / 2.0) < (a.goalWidth / 2.0)

        ( puck, paddle1, paddle2, score1, score2 ) =
            if goal1 then
                ( recenter 0.4 model.puck, model.paddle1, model.paddle2, model.score1 + 1, model.score2 )
            else if goal2 then
                ( recenter -0.4 model.puck, model.paddle1, model.paddle2, model.score1, model.score2 + 1 )
            else
                let
                    puck_0 =
                        bounce model.puckBounds dt model.puck

                    paddle1_0 =
                        bump model.paddle1Bounds dt model.paddle1

                    paddle2_0 =
                        bump model.paddle2Bounds dt model.paddle2

                    bodies =
                        collisions dt [ puck_0, paddle1_0, paddle2_0 ]

                    ( puck, paddle1, paddle2 ) =
                        case bodies of
                            [ b1, b2, b3 ] ->
                                ( b1, b2, b3 )

                            _ ->
                                ( model.puck, model.paddle1, model.paddle2 )

                    puck_1 =
                        { puck | velocity = v3_clamp model.attributes.puckMaxSpeed puck.velocity }
                in
                    ( puck_1, paddle1, paddle2, model.score1, model.score2 )
    in
        { model
            | puck = puck
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
        setElevation pos =
            V3.setY (model.attributes.tableHeight + ground.elevation pos) pos

        robot =
            updateRobotPaddle dt model.robotPaddle2

        paddle2 =
            applyRobotPaddle robot model.puck model.paddle2

        newModel =
            { model
                | paddle2 = paddle2
                , robotPaddle2 = robot
            }
    in
        setPosition (setElevation model.attributes.position) (collide dt newModel)


bodies : Model -> Vec3 -> List Body
bodies model pos =
    if (V3.distanceSquared model.attributes.position pos < 900) then
        model.table :: List.map toBody [ model.puck, model.paddle1, model.paddle2 ]
    else
        [ model.table ]


framing : PartyKey -> Model -> Maybe Framing
framing partyKey model =
    let
        target =
            { position = model.paddle1.position
            , orientation = model.attributes.orientation
            , velocity = model.paddle1.velocity
            }

        povTargetPos =
            V3.add model.table.position <|
                V3.scale (0.3 * V3.dot (V3.sub model.puck.position model.table.position) V3.k) V3.k

        puck =
            model.puck

        povTarget =
            Camera.toTarget { puck | position = povTargetPos }

        pov =
            Camera.rollUpright <|
                Camera.retarget povTarget
                    { position =
                        V3.add (vec3 0 (model.attributes.tableWidth / 1.2) 0) <|
                            V3.sub model.table.position <|
                                V3.scale (1.8 * model.attributes.tableLength / 2.0)
                                    (Orientation.rotateLabV model.attributes.orientation V3.k)
                    , orientation = Orientation.initial
                    , target = povTarget
                    , fovy = 45
                    }
    in
        Just
            { target = target
            , pov = pov
            }


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
