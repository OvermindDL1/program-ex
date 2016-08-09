module ProgramEx.States exposing
  ( States
  , Callbacks(..)
  , Delegates
  , defaultOn, defaultOff
  , enableAll, disableAll
  , enableOnly, disableOnly
  , enable, disable
  , delegate
  , doUpdate, doSubscriptions, doView, getDelegates
  )

{-|
This module handles the States structure that controls how callbacks will be called

Example usage via in a filter callback:

    filters : Msg -> Model -> ( Msg, States Model Msg )
    filters msg model =
        let
            log =
                debug_log (model.programFlags.debug_log |> Maybe.withDefault False) "filters" ( msg, model )
        in
            case msg of
                Helpers helpersMsg ->
                    ( msg
                    , States.enableAll |> States.delegate (helpers_delegate Helpers helpersMsg)
                    )

                Mdl mdlMsg ->
                    ( msg
                    , States.enableAll
                        |> States.delegate
                            { key = "Mdl"
                            , update = Just (\_ o -> Material.update mdlMsg o)
                            , subscriptions = Just (\o -> Material.subscriptions Mdl o)
                            }
                    )

                MesgList_Scroll scrolled ->
                    case model.loc of
                        RoomLocation rid _ ->
                            let
                                doLoad : Bool
                                doLoad =
                                    (model.firstMessageReached == False)
                                        && (scrolled.pos < 16)
                                        && (model.isLoadingOlder == False)
                                        && ((Dict.size model.active_room_msgs) >= 10)
                            in
                                if doLoad then
                                    ( MesgList_LoadOlder rid, States.enableAll )
                                else
                                    ( msg, States.disableAll )

                        _ ->
                            ( msg, States.disableAll )

                _ ->
                    ( msg, States.enableAll )


@docs States, Callbacks, Delegates
@docs defaultOn, defaultOff
@docs enableAll, disableAll
@docs enableOnly, disableOnly
@docs enable, disable
@docs delegate
@docs doUpdate, doSubscriptions, doView, getDelegates
-}



{-| Delegate record, fill this in to specify delegates for a module
-}
type alias Delegates userModel userMsg =
  { key : String
  , update : Maybe ( userMsg -> userModel -> ( userModel, Cmd userMsg ) )
  , subscriptions : Maybe ( userModel -> Sub userMsg )
  }


{-| Holds the State information
-}
type States userModel userMsg =
  S { doUpdate : Bool
    , doSubscriptions : Bool
    , doView : Bool
    , callbackDelegates : Maybe (Delegates userModel userMsg)
    }


{-| Various supported states that can be allowed or denied
-}
type Callbacks
  = Update
  | Subscriptions
  | View


{-| Gets the default state set, everything is enabled
-}
defaultOn : States userModel userMsg
defaultOn =
  S { doUpdate=True
    , doSubscriptions=True
    , doView=True
    , callbackDelegates=Nothing
    }



{-| Gets the default state set, everything is disabled
-}
defaultOff : States userModel userMsg
defaultOff =
  S { doUpdate=False
    , doSubscriptions=False
    , doView=False
    , callbackDelegates=Nothing
    }


{-| Gets from the state if the update callback should be run
-}
doUpdate : States userModel userMsg -> Bool
doUpdate (S states) = states.doUpdate


{-| Gets from the state if the subscriptions callback should be run
-}
doSubscriptions : States userModel userMsg -> Bool
doSubscriptions (S states) = states.doSubscriptions


{-| Gets from the state if the view callback should be run
-}
doView : States userModel userMsg -> Bool
doView (S states) = states.doView


{-| Gets the delegates for this state
-}
getDelegates : States userModel userMsg -> Maybe (Delegates userModel userMsg)
getDelegates (S states) =
  states.callbackDelegates


-- User userfuls


{-| Runs all callbacks as expected, same as `defaultOn`
-}
enableAll : States userModel userMsg
enableAll =
  defaultOn


{-| Runs no callbacks at all, same as `defaultOff`
-}
disableAll : States userModel userMsg
disableAll =
  defaultOff


{-| Returns a State that will only allow these callbacks to run
-}
enableOnly : List Callbacks -> States userModel userMsg
enableOnly only =
  List.foldl enable disableAll only


{-| Returns a State that will only allow all callbacks except the ones specified
-}
disableOnly : List Callbacks -> States userModel userMsg
disableOnly only =
  List.foldl disable enableAll only


{-| Returns a new state from an old enabling a specific callback
-}
enable : Callbacks -> States userModel userMsg -> States userModel userMsg
enable state (S states) =
  case state of
    Update -> S { states | doUpdate = True }
    Subscriptions -> S { states | doSubscriptions = True }
    View -> S { states | doView = True }



{-| Returns a new state from an old disabling a specific callback
-}
disable : Callbacks -> States userModel userMsg -> States userModel userMsg
disable state (S states) =
  case state of
  Update -> S { states | doUpdate = True }
  Subscriptions -> S { states | doSubscriptions = True }
  View -> S { states | doView = True }


{-| Changes the callback functions for these callbacks to the specified ones, useful for TEA modules delegation

Example use:

    filters : Msg -> Model -> ( Msg, States )
    filters msg model =
      case msg of
        Material
-}
delegate : Delegates userModel userMsg -> States userModel userMsg -> States userModel userMsg
delegate  delegates (S states) =
  S { states | callbackDelegates = Just delegates
    }
