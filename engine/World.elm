module World exposing (WorldModel, create)

import Html exposing (Html)
import Bag exposing (Bag)
import Time exposing (Time)
import Space
import Control exposing (..)
import Maybe.Extra exposing (isJust)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)
import Model exposing (Args, WorldKey(..), AppKey(..), PartyKey(..))
import Body exposing (Body)
import Camera exposing (Framing, Shot)
import App exposing (..)
import Ground exposing (Ground)
import Math.Vector3 as V3 exposing (vec3)
import Task

type alias WorldModel a =
    { worldModel : a
    , worlds : Bag Stuff
    }

type alias Stuff =
    { maybeGround : Maybe Ground
    , apps : Bag App
    , parties : Bag Party
    , defaultSelf : ( App, Cmd AppMsg )
    }

type alias Party =
    { rideKey : Maybe AppKey
    , self : App -- App to be when not riding
    -- , focusKey : Bag.Key
    }


create :
    ( model, Cmd msg )
    -> (msg -> model -> ( model, Cmd msg ))
    -> List { apps : List ( App, Cmd AppMsg ), defaultSelf : ( App, Cmd AppMsg ) }
    -> Program Args (Model.Model (WorldModel model) (WorldMsg msg)) (Model.Msg (WorldMsg msg))
create hubInit hubUpdate details =
    Space.programWithFlags
        { init = worldInit hubInit details
        , view = worldView
        , update = worldUpdate hubUpdate
        , label = worldLabel
        , overlay = worldOverlay
        , animate = worldAnimate
        , join = worldJoin
        , leave = worldLeave
        , changeRide = worldChangeRide
        , framing = worldFraming
        , focus = worldFocus
        , ground = worldGround
        }


worldApp : WorldKey AppKey -> WorldModel a -> Maybe App
worldApp (WorldKey worldKey (AppKey appKey)) model =
    Bag.get worldKey model.worlds
    |> Maybe.map .apps
    |> Maybe.andThen (Bag.get appKey)

worldParty : WorldKey PartyKey -> WorldModel a -> Maybe Party
worldParty (WorldKey worldKey (PartyKey partyKey)) model =
    Bag.get worldKey model.worlds
    |> Maybe.map .parties
    |> Maybe.andThen (Bag.get partyKey)

worldApps : WorldKey () -> List ( App, Cmd AppMsg ) -> ( Bag App, Cmd (WorldMsg a) )
worldApps (WorldKey worldKey ()) appsList =
    let
        f ( newApps, newCmdMsg ) ( oldBag, oldCmdMsgs ) =
            let
                ( key, newBag ) =
                    Bag.insert newApps oldBag
            in
                ( newBag, oldCmdMsgs ++ [ Cmd.map (Send (ToApp (WorldKey worldKey (AppKey key)))) newCmdMsg ] )

        ( appsBag, unbatched ) =
            List.foldl f ( Bag.empty, [] ) appsList

        worldCmds =
            List.map (Cmd.map (toWorldMsg (WorldKey worldKey ()))) unbatched
    in
        ( appsBag, Cmd.batch worldCmds )

oneWorldInit :
    { apps : List ( App, Cmd AppMsg ), defaultSelf : ( App, Cmd AppMsg ) }
    -> ( Stuff, Cmd (WorldMsg msg) )
oneWorldInit detail =
    let
        ( appsBag, appCmds ) =
            worldApps (WorldKey 0 ()) detail.apps
    in
        ( { maybeGround = Nothing
          , apps = appsBag
          , parties = Bag.empty
          , defaultSelf = detail.defaultSelf
          }
        , appCmds
        )

worldInit :
    ( model, Cmd msg )
    -> List { apps : List ( App, Cmd AppMsg ), defaultSelf : ( App, Cmd AppMsg ) }
    -> ( WorldModel model, Cmd (WorldMsg msg) )
worldInit hubInit details =
    let
        ( hubModel, hubCmd ) =
            hubInit

        ( worldsBag, worldsCmds ) =
            case details of
                [] ->
                    ( Bag.empty, Cmd.none )

                ( detail :: _ ) ->
                    let (stuff, cmd) = oneWorldInit detail
                        (_, bag) = Bag.insert stuff Bag.empty
                    in ( bag, cmd)
    in
        ( { worldModel = hubModel
          , worlds = worldsBag
          }
        , Cmd.batch
            [ Cmd.map Hub hubCmd
            , worldsCmds
            ]
        )


worldView : WorldKey () -> WorldModel model -> Maybe Model.World
worldView (WorldKey worldKey ()) model =
    let
        mStuff = Bag.get worldKey model.worlds
    in
        case Maybe.andThen .maybeGround mStuff of
            Nothing ->
                Nothing

            Just ground ->
                Maybe.map (makeWorld ground) mStuff


makeWorld : Ground -> Stuff -> Model.World
makeWorld ground stuff =
    let
        partyBodies party =
            case party.rideKey of
                Just _ -> []
                Nothing -> bodies party.self

        worldBodies =
            List.concatMap bodies (Bag.items stuff.apps) ++
            List.concatMap partyBodies (Bag.items stuff.parties)
    in
        { bodies = worldBodies, ground = ground }


toWorldMsg :
    WorldKey ()
    -> DispatchHub Route (EffectMsg ()) Msg Dynamic a
    -> DispatchHub Route (EffectMsg (WorldKey ())) Msg Dynamic a
toWorldMsg worldKey msg =
    let
        toWorldEffect e =
            case e of
                UpdateGround () ground ->
                    UpdateGround worldKey ground

                RelocateParty () partyKey location ->
                    RelocateParty worldKey partyKey location

        toWorldDispatch d =
            case d of
                Self nodeMsg -> Self nodeMsg
                Ctrl ctrlMsg -> Ctrl ctrlMsg
                Effect e -> Effect (toWorldEffect e)
    in
        case msg of
            Hub hubMsg ->
                Hub hubMsg
            Send key dispatch ->
                Send key (toWorldDispatch dispatch)
            Forward key ctrlMsg ->
                Forward key ctrlMsg
            HubEff e ->
                HubEff (toWorldEffect e)

toAppMsg : Dispatch (EffectMsg (WorldKey ())) Msg Dynamic -> AppMsg
toAppMsg dispatch =
    let
        toAppEffect e =
            case e of
                UpdateGround _ ground ->
                    UpdateGround () ground

                RelocateParty _ partyKey location ->
                    RelocateParty () partyKey location
    in
        case dispatch of
            Self nodeMsg -> Self nodeMsg
            Ctrl ctrlMsg -> Ctrl ctrlMsg
            Effect e -> Effect (toAppEffect e)

worldUpdate :
    (msg -> model -> ( model, Cmd msg ))
    -> WorldMsg msg
    -> WorldModel model
    -> ( WorldModel model, Cmd (WorldMsg msg) )
worldUpdate hubUpdate msg model =
    case msg of
        Hub hubMsg ->
            let
                ( hubModel, hubCmd ) =
                    hubUpdate hubMsg model.worldModel
            in
                ( { model | worldModel = hubModel }, Cmd.map Hub hubCmd )

        HubEff (Control.UpdateGround (WorldKey worldKey ()) ground) ->
            let
                updateGround stuff =
                    { stuff | maybeGround = Just ground }
            in
                ( { model | worlds = Bag.update worldKey (Maybe.map updateGround) model.worlds }
                , Cmd.none
                )

        HubEff (Control.RelocateParty (WorldKey worldKey ()) (PartyKey partyKey) location) ->
            let
                leaveRide party =
                    { party | rideKey = Nothing
                            , self = App.reposition (Just location) party.self
                    }

                updateParties stuff =
                    { stuff | parties = Bag.update partyKey (Maybe.map leaveRide) stuff.parties }
            in
                ( { model | worlds = Bag.update worldKey (Maybe.map updateParties) model.worlds }
                , Cmd.none
                )

        Send key appMsg ->
            let
                (mApp, worldKey, updateModel) = case key of
                    ToApp (WorldKey worldKey (AppKey appKey)) ->
                        ( worldApp (WorldKey worldKey (AppKey appKey)) model
                        , worldKey
                        , \newApp ->
                              let
                                  updateApps stuff = { stuff | apps = Bag.replace appKey newApp stuff.apps }
                              in
                                  { model | worlds = Bag.update worldKey (Maybe.map updateApps) model.worlds }
                        )

                    ToParty (WorldKey worldKey (PartyKey partyKey)) ->
                        let
                            u newSelf party = { party | self = newSelf }
                        in
                            ( Maybe.map .self <| worldParty (WorldKey worldKey (PartyKey partyKey)) model
                            , worldKey                       
                            , \newSelf ->
                                let
                                    updateParties stuff = { stuff | parties = Bag.update partyKey (Maybe.map (u newSelf)) stuff.parties }
                                in
                                  { model | worlds = Bag.update worldKey (Maybe.map updateParties) model.worlds }
                            )

                response x =
                    case x of
                        Effect e ->
                            case e of
                                UpdateGround () ground ->
                                    HubEff (UpdateGround (WorldKey worldKey ()) ground)

                                RelocateParty () partyKey location ->
                                    HubEff (RelocateParty (WorldKey worldKey ()) partyKey location)

                        m ->
                            toWorldMsg (WorldKey worldKey ()) (Send key m)

            in
                case mApp of
                    Nothing ->
                        ( model, Cmd.none )

                    Just app ->
                        let
                            ( appModel, appCmdMsg ) =
                                App.update (toAppMsg appMsg) app

                        in
                            ( updateModel appModel
                            , Cmd.map response appCmdMsg
                            )


        Forward (ToParty (WorldKey worldKey (PartyKey partyKey))) fwdMsg ->
            case worldParty (WorldKey worldKey (PartyKey partyKey)) model of
                Just party ->
                    case party.rideKey of
                        Just (AppKey rideKey) ->
                            case worldApp (WorldKey worldKey (AppKey rideKey)) model of
                                Just t ->
                                    let
                                        ( appModel, appCmdMsg ) =
                                            App.update (Ctrl fwdMsg) t
                                        updateApps stuff = { stuff | apps = Bag.replace rideKey appModel stuff.apps }
                                        newModel =
                                              { model | worlds = Bag.update worldKey (Maybe.map updateApps) model.worlds }
                                    in
                                        ( newModel
                                        , Cmd.map (Send (ToApp (WorldKey worldKey (AppKey rideKey))) >> toWorldMsg (WorldKey worldKey())) appCmdMsg
                                        )
                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            let
                                ( appModel, appCmdMsg ) =
                                    App.update (Ctrl fwdMsg) party.self
                                u newSelf party = { party | self = newSelf }
                                updateParties stuff = { stuff | parties = Bag.update partyKey (Maybe.map (u appModel)) stuff.parties }
                                newModel =
                                    { model | worlds = Bag.update worldKey (Maybe.map updateParties) model.worlds }
                            in
                                ( newModel
                                , Cmd.map (Send (ToParty (WorldKey worldKey (PartyKey partyKey))) >> toWorldMsg (WorldKey worldKey())) appCmdMsg
                                )
                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


worldAnimate : WorldKey () -> Ground -> Time -> WorldModel a -> WorldModel a
worldAnimate (WorldKey worldKey ()) ground dt model =
    let
        updateApps stuff =
            { stuff | apps = Bag.map (App.animate ground dt) stuff.apps }
    in
        { model | worlds = Bag.update worldKey (Maybe.map updateApps) model.worlds }


worldJoin : WorldKey () -> WorldModel model -> (Maybe (WorldKey PartyKey), WorldModel model, Cmd (WorldMsg msg))
worldJoin (WorldKey worldKey ()) model =
    let
        -- freshParty : Stuff -> (Party, Cmd msg)
        freshParty stuff =
            let
                ( defaultSelfApp, defaultSelfCmd ) = stuff.defaultSelf
            in
                ( { rideKey = Nothing
                  , self = defaultSelfApp
                  }
                , defaultSelfCmd
                )

        -- thisFreshParty : Maybe (Party, Cmd msg)
        thisFreshParty =
            Bag.get worldKey model.worlds
            |> Maybe.map freshParty

        -- foo : Bag Party -> ( WorldKey PartyKey, Bag Party )
        insertParty newParty parties =
            let
                ( partyKey, newParties ) =
                    Bag.insert newParty parties
                worldPartyKey = WorldKey worldKey (PartyKey partyKey)
            in
                ( worldPartyKey, newParties )

        -- updateParties :
        --     (Bag Party -> (WorldKey PartyKey, Bag Party, Cmd msg))
        --     -> Stuff -> (WorldKey PartyKey, Stuff, Cmd msg)
        updateParties f stuff =
            let
                (worldPartyKey, newParties) = f stuff.parties
            in
                (worldPartyKey, { stuff | parties = newParties })

    in
        case Bag.get worldKey model.worlds of
            Just stuff ->
                let
                    (newParty, selfCmd) = freshParty stuff
                    (worldPartyKey, newStuff) = updateParties (insertParty newParty) stuff
                    newModel = { model | worlds = Bag.replace worldKey newStuff model.worlds }
                in
                    ( Just worldPartyKey, newModel
                    , Cmd.map (Send (ToParty worldPartyKey) >> toWorldMsg (WorldKey worldKey ())) selfCmd )
            Nothing ->
                (Nothing, model, Cmd.none)


worldLeave : WorldKey PartyKey -> WorldModel a -> WorldModel a
worldLeave (WorldKey worldKey (PartyKey partyKey)) model =
    let
        updateParties f stuff = { stuff | parties = f stuff.parties }
    in
        { model | worlds = Bag.update worldKey (Maybe.map (updateParties (Bag.remove partyKey))) model.worlds }

worldChangeRide : WorldKey PartyKey -> WorldModel model -> ( WorldModel model, Cmd (WorldMsg msg) )
worldChangeRide (WorldKey worldKey (PartyKey partyKey)) model =
    let
        updateRide party =
            case (party.rideKey, App.framing party.self) of
                (Just (AppKey rideKey), _) ->
                    let
                        positioning x = { position = x.position, orientation = x.orientation }
                        ridePos =
                            Maybe.andThen App.framing (worldApp (WorldKey worldKey (AppKey rideKey)) model)
                            |> Maybe.map (.pov >> positioning)

                        cmd = Cmd.map (Send (ToApp ((WorldKey worldKey (AppKey rideKey)))))
                              (Task.succeed (PartyKey partyKey) |> Task.perform (Ctrl << Leave))
                    in
                        ( { party | rideKey = Nothing
                                  , self = App.reposition ridePos party.self
                          }
                        , cmd
                        )

                (Nothing, Just myFraming) ->
                    let
                        myPos = myFraming.pov.position
                        myDir = Model.direction myFraming.pov

                        secondPosition (k, app) =
                            case App.framing app of
                                Just framing -> Just (k, framing.target.position)
                                Nothing -> Nothing

                        relativePosition appPos =
                            V3.sub appPos myPos

                        inFrontOf relPos =
                            V3.dot relPos myDir > 0

                        mClosestKey =
                            Bag.get worldKey model.worlds
                            |> Maybe.map .apps
                            |> Maybe.withDefault Bag.empty
                            |> Bag.toList
                            |> List.filterMap secondPosition
                            |> List.map (Tuple.mapSecond relativePosition)
                            |> List.filter (Tuple.second >> inFrontOf)
                            |> List.map (Tuple.mapSecond V3.lengthSquared)
                            |> List.filter (Tuple.second >> (\d -> d < 10*10))
                            |> List.sortBy Tuple.second
                            |> List.head
                            |> Maybe.map Tuple.first
                            |> Maybe.map AppKey

                        cmd =
                            case mClosestKey of
                                Just (AppKey rideKey) ->
                                    Cmd.map (Send (ToApp (WorldKey worldKey (AppKey rideKey))))
                                    (Task.succeed (PartyKey partyKey) |> Task.perform (Ctrl << Enter))
                                Nothing ->
                                    Cmd.none
                    in
                        ( { party | rideKey = mClosestKey }
                        , cmd
                        )

                _ ->
                        ( party, Cmd.none )
    in
        let mNewPartyCmds = Maybe.map updateRide <| worldParty (WorldKey worldKey (PartyKey partyKey)) model
            mNewParty = Maybe.map Tuple.first mNewPartyCmds
            newCmds =
                Maybe.map Tuple.second mNewPartyCmds
                |> Maybe.withDefault Cmd.none

            updateParties stuff =
                { stuff | parties = Bag.update partyKey (always mNewParty) stuff.parties }
        in
            ( { model | worlds = Bag.update worldKey (Maybe.map updateParties) model.worlds }
            , newCmds
            )

worldLabel : WorldKey PartyKey -> WorldModel a -> String
worldLabel worldPartyKey model =
    let
        (WorldKey worldKey (PartyKey _)) = worldPartyKey

        none =
            "<>"
    in
        case worldParty worldPartyKey model of
            Just party ->
                case party.rideKey of
                    Just (AppKey appKey) ->
                        case worldApp (WorldKey worldKey (AppKey appKey)) model of
                            Just app ->
                                App.label app

                            Nothing ->
                                "Ride not found"

                    Nothing ->
                        App.label party.self

            Nothing ->
                "Party not found"


worldOverlay : WorldKey PartyKey -> WorldModel a -> Html (WorldMsg msg)
worldOverlay worldPartyKey model =
    let
        (WorldKey worldKey (PartyKey _)) = worldPartyKey

        none =
            Html.text "Welcome to DreamBuggy"
    in
        case worldParty worldPartyKey model of
            Just party ->
                case party.rideKey of
                    Just (AppKey appKey) ->
                        let worldAppKey = WorldKey worldKey (AppKey appKey)
                        in
                            case worldApp worldAppKey model of
                                Just app ->
                                    Html.map (Send (ToApp worldAppKey) >> toWorldMsg (WorldKey worldKey ())) (App.overlay app)

                                Nothing ->
                                    Html.text "App not found"

                    Nothing ->
                        Html.map (Send (ToParty worldPartyKey) >> toWorldMsg (WorldKey worldKey())) (App.overlay party.self)

            Nothing ->
                Html.text "Party not found"


worldFraming : WorldKey PartyKey -> WorldModel a -> Maybe Framing
worldFraming worldPartyKey model =
    let
        (WorldKey worldKey (PartyKey _)) = worldPartyKey
    in
        case worldParty worldPartyKey model of
            Just party ->
                case party.rideKey of
                    Just (AppKey appKey) ->
                        case worldApp (WorldKey worldKey (AppKey appKey)) model of
                            Just app ->
                                App.framing app

                            Nothing ->
                                Nothing

                    Nothing ->
                        App.framing party.self

            Nothing ->
                Nothing


worldFocus : WorldKey AppKey -> WorldModel a -> Maybe Focus
worldFocus appKey model =
    case worldApp appKey model of
        Just app ->
            App.focus app

        _ ->
            Nothing


worldGround : WorldKey () -> WorldModel model -> Maybe Ground
worldGround (WorldKey worldKey ()) model =
    Bag.get worldKey model.worlds
    |> Maybe.andThen .maybeGround
