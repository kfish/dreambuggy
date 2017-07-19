module Object
    exposing
        ( create
        , texturedObj
        , texturedObjWith
        , reflectiveObj
        , reflectiveObjWith
        )

import App exposing (..)
import App.Control exposing (..)
import Appearance exposing (Appearance)
import Body exposing (..)
import Dispatch exposing (..)
import Html exposing (Html)
import Html.Attributes as Html
import Location exposing (Scale(..))
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Object.Attributes exposing (..)
import Object.ReflectiveObj as ReflectiveObj exposing (ReflectiveObjAttributes)
import Object.TexturedObj as TexturedObj exposing (TexturedObjAttributes)
import Object.Types exposing (Load(..))
import Object.Util exposing (scaleToVec3)
import Object.Wrapper exposing (..)
import Orientation exposing (Orientation)
import Setter exposing (..)


type alias Model vehicle =
    { motion : Moving {}
    , rotation : Maybe Orientation
    , action : Action vehicle
    , body : Maybe (Moving Body)
    , object : Load ObjectResult
    , dimensions : Vec3
    }


type alias Msg =
    ObjectMsg


texturedObj : String -> String -> String -> ObjectAttributes
texturedObj meshPath diffuseTexturePath normalTexturePath =
    TexturedObj.texturedObj meshPath diffuseTexturePath normalTexturePath
        |> TexturedObj


texturedObjWith :
    String
    -> String
    -> String
    -> List (Update TexturedObjAttributes)
    -> ObjectAttributes
texturedObjWith meshPath diffuseTexturePath normalTexturePath updates =
    TexturedObj.texturedObj meshPath diffuseTexturePath normalTexturePath
        |> applyUpdates updates
        |> TexturedObj


reflectiveObj : String -> String -> ObjectAttributes
reflectiveObj meshPath reflectionTexturePath =
    ReflectiveObj.reflectiveObj meshPath reflectionTexturePath
        |> ReflectiveObj


reflectiveObjWith :
    String
    -> String
    -> List (Update ReflectiveObjAttributes)
    -> ObjectAttributes
reflectiveObjWith meshPath reflectionTexturePath updates =
    ReflectiveObj.reflectiveObj meshPath reflectionTexturePath
        |> applyUpdates updates
        |> ReflectiveObj


create : List (Update (Attributes vehicle Msg)) -> ( App, Cmd AppMsg )
create updates =
    let
        create_ attributes =
            App.create (init attributes)
                { id = always attributes.id
                , label = always attributes.label
                , update = update attributes.scale attributes.action
                , animate = animate
                , bodies = bodies
                , framing = framing
                , focus = focus
                , overlay = always attributes.overlay
                , reposition = reposition
                }
    in
        create_ (applyUpdates updates defaultAttributes)


applyMotion : Model vehicle -> Model vehicle
applyMotion model =
    let
        orientation o =
            model.rotation
                |> Maybe.map (Orientation.unwind o)
                |> Maybe.withDefault o

        apply motion thing =
            { thing
                | position = motion.position
                , orientation = orientation motion.orientation
                , velocity = motion.velocity
            }
    in
        { model | body = Maybe.map (apply model.motion) model.body }


setMotion : Moving {} -> Model vehicle -> Model vehicle
setMotion motion model =
    applyMotion { model | motion = motion }


loadBody :
    Scale
    -> ( Load ObjectResult, Cmd ObjectMsg )
    -> Model vehicle
    -> ( Model vehicle, Cmd (CtrlMsg Msg) )
loadBody scale ( newObject, newMsg ) model =
    let
        ( mBody, dimensions ) =
            case newObject of
                Loading _ ->
                    ( Nothing, vec3 1 1 1 )

                Ready appear dimensions ->
                    ( Just
                        { anchor = AnchorGround
                        , scale = scaleToVec3 dimensions scale
                        , position = vec3 0 0 0
                        , orientation = Orientation.initial
                        , appear = appear
                        , velocity = vec3 0 0 0
                        }
                    , dimensions
                    )
    in
        ( applyMotion
            { model
                | object = newObject
                , body = mBody
                , dimensions = dimensions
            }
        , Cmd.map Self newMsg
        )


init : Attributes vehicle Msg -> ( Model vehicle, Cmd (CtrlMsg Msg) )
init attributes =
    let
        ( object, objectCmds ) =
            objectInit attributes.object
    in
        loadBody attributes.scale
            ( object, objectCmds )
            { motion =
                { position = attributes.position
                , orientation = Orientation.initial
                , velocity = vec3 0 0 0
                }
            , rotation = attributes.rotation
            , action = attributes.action
            , body = Nothing
            , object = object
            , dimensions = vec3 1 1 1
            }


update :
    Scale
    -> Action vehicle
    -> CtrlMsg Msg
    -> Model vehicle
    -> ( Model vehicle, Cmd (CtrlMsg Msg) )
update scale action msg model =
    case msg of
        Self m ->
            loadBody scale (objectUpdate m model.object) model

        Ctrl (Enter partyKey) ->
            case action of
                Portal location ->
                    ( model, teleport partyKey location )

                _ ->
                    ( model, Cmd.none )

        Ctrl (Move dp) ->
            -- ( mapBody (translate dp), Cmd.none)
            ( model, Cmd.none )

        Ctrl (Drive ground inputs) ->
            case action of
                Vehicle v ->
                    ( setMotion (v.drive v.vehicle model.dimensions ground inputs model.motion) model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


animate : Ground -> Time -> Model vehicle -> Model vehicle
animate ground dt model =
    let
        aboveGround pos =
            let
                minY =
                    ground.elevation pos
            in
                if V3.getY pos > minY then
                    pos
                else
                    V3.setY minY pos

        motion =
            model.motion

        newMotion =
            { motion
                | position = aboveGround motion.position
            }
    in
        setMotion newMotion model


bodies : Model vehicle -> Vec3 -> List Body
bodies model pos =
    case model.body of
        Just body ->
            [ toBody body ]

        Nothing ->
            []


reposition : Maybe AppPosition -> Model vehicle -> Model vehicle
reposition mPos model =
    case mPos of
        Just pos ->
            let
                motion =
                    model.motion

                newMotion =
                    { motion
                        | position = pos.position
                        , orientation = pos.orientation
                    }
            in
                setMotion newMotion model

        Nothing ->
            model


framing : PartyKey -> Model vehicle -> Maybe Framing
framing _ model =
    Maybe.map (always (toFraming model.motion)) model.body


focus : Model vehicle -> Maybe Focus
focus model =
    Maybe.map appToFocus model.body



{-
   overlay : Model vehicle -> Html msg
   overlay model =
       let
           textLeft =
               Html.style [ ( "text-align", "left" ) ]
       in
           Html.div []
               [ Html.h2 []
                   [ Html.text model.label ]
               , Html.text "A statue"
               ]
-}
