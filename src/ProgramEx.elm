module ProgramEx exposing (programExBuilderWithFlags, programExBuilderWithFlagsAndNavigation)

{-|
@docs programExBuilderWithFlags, programExBuilderWithFlagsAndNavigation
-}

import ProgramEx.States as States exposing (States, doUpdate, doSubscriptions, doView)
import Html
import Dict


type alias Model userModel userMsg =
    { userModel : userModel
    , view : Html.Html userMsg
    , subscriptions : Dict.Dict String (Sub userMsg)
    }


flagsInit :
  { app
  | init : flags -> ( userModel, Cmd userMsg )
  , view : userModel -> Html.Html userMsg
  , subscriptions : userModel -> Sub userMsg
  } -> flags -> ( Model userModel userMsg, Cmd userMsg )
flagsInit app flags =
    let
        -- stateInit : ( userModel, Cmd userMsg ) -- Broken in Elm right now due to annotation names not being shared in outer/inner scopes
        stateInit =
            app.init flags

        ( initModel, initCmd ) =
            stateInit
    in
        ( { userModel = initModel
          , view = app.view initModel
          , subscriptions = Dict.fromList [("", app.subscriptions initModel)]
          }
        , initCmd
        )


flagsNavInit :
  { app
  | init : flags -> navParseResult -> ( userModel, Cmd userMsg )
  , view : userModel -> Html.Html userMsg
  , subscriptions : userModel -> Sub userMsg
  } -> flags -> navParseResult -> ( Model userModel userMsg, Cmd userMsg )
flagsNavInit app flags navData =
    let
        -- stateInit : ( userModel, Cmd userMsg ) -- Broken in Elm right now due to annotation names not being shared in outer/inner scopes
        stateInit =
            app.init flags navData

        ( initModel, initCmd ) =
            stateInit
    in
        ( { userModel = initModel
          , view = app.view initModel
          , subscriptions = Dict.fromList [("", app.subscriptions initModel)]
          }
        , initCmd
        )


update :
    (userMsg -> userModel -> ( userMsg, States userModel userMsg ))
    -> (userMsg -> userModel -> ( userModel, Cmd userMsg ))
    -> (userModel -> Html.Html userMsg)
    -> (userModel -> Sub userMsg)
    -> userMsg
    -> Model userModel userMsg
    -> ( Model userModel userMsg, Cmd userMsg )
update userFilters userUpdate userView userSubscriptions msg model =
    let
        -- stateFiltered : ( userMsg, States ) -- Broken in Elm right now due to annotation names not being shared in outer/inner scopes
        stateFiltered =
            userFilters msg model.userModel

        ( transMsg, states ) =
            stateFiltered

        delegates =
            case States.getDelegates states of
                Just delegates ->
                    delegates

                Nothing ->
                    { key = ""
                    , update = Nothing
                    , subscriptions = Nothing
                    }

        delegatesKey =
            delegates.key

        delegatesUpdate =
            delegates.update |> Maybe.withDefault userUpdate

        delegatesSubscriptions =
            delegates.subscriptions |> Maybe.withDefault userSubscriptions

        -- updateData : ( userModel, Cmd userMsg ) -- Broken in Elm right now due to annotation names not being shared in outer/inner scopes
        updateData =
            if doUpdate states then
                delegatesUpdate transMsg model.userModel
            else
                ( model.userModel, Cmd.none )

        ( updateModel, updateCmd ) =
            updateData

        -- viewData : Html.Html userMsg -- Broken in Elm right now due to annotation names not being shared in outer/inner scopes
        viewData =
            if doView states then
                userView updateModel
            else
                model.view

        -- subscriptionsData : Dict String (Sub userMsg) -- Broken in Elm right now due to annotation names not being shared in outer/inner scopes
        subscriptionsData =
            if doSubscriptions states then
                Dict.insert delegatesKey (delegatesSubscriptions updateModel) model.subscriptions
            else
                model.subscriptions

        newModel =
            { model
                | userModel = updateModel
                , view = viewData
                , subscriptions = subscriptionsData
            }
    in
        if doUpdate states then
            ( newModel
            , updateCmd
            )
        else
            ( newModel, Cmd.none )


view : Model userModel userMsg -> Html.Html userMsg
view model =
    model.view


subscriptions : Model userModel userMsg -> Sub userMsg
subscriptions model =
    Dict.toList model.subscriptions
        |> List.map (\( k, v ) -> v)
        |> Sub.batch


urlUpdate : (navData -> userModel -> ( userModel, Cmd userMsg )) -> navData -> Model userModel userMsg -> ( Model userModel userMsg, Cmd userMsg )
urlUpdate userUrlUpdate navData model =
    let
        -- urlUpdateData : ( userModel, Cmd userMsg ) -- Broken in Elm right now due to annotation names not being shared in outer/inner scopes
        urlUpdateData =
            userUrlUpdate navData model.userModel

        ( userModel, userCmd ) =
            urlUpdateData
    in
        ( { model | userModel = userModel }
        , userCmd
        )


{-|
Pass in the usual callbacks and it returns a built callback set.

This can be used via Html.App.programWithFlags like:

    Html.App.programWithFlags
      ( programExBuilderWithFlags
        { init=init
        , filter=filter
        , update=update
        , view=view
        , subscriptions=subscriptions
        }
      )
-}
programExBuilderWithFlags :
    { init : flags -> ( userModel, Cmd userMsg )
    , filters : userMsg -> userModel -> ( userMsg, States userModel userMsg )
    , update : userMsg -> userModel -> ( userModel, Cmd userMsg )
    , view : userModel -> Html.Html userMsg
    , subscriptions : userModel -> Sub userMsg
    }
    -> { init : flags -> ( Model userModel userMsg, Cmd userMsg )
       , update : userMsg -> Model userModel userMsg -> ( Model userModel userMsg, Cmd userMsg )
       , view : Model userModel userMsg -> Html.Html userMsg
       , subscriptions : Model userModel userMsg -> Sub userMsg
       }
programExBuilderWithFlags app =
    { init = flagsInit app
    , update = update app.filters app.update app.view app.subscriptions
    , view = view
    , subscriptions = subscriptions
    }


{-|
Pass in the usual callbacks and it returns a built callback set.

This can be used via Html.App.programWithFlags like:

    Navigation.programWithFlags urlParser
      ( programExBuilderWithFlagsAndNavigation
        { init=init
        , filter=filter
        , update=update
        , view=view
        , subscriptions=subscriptions
        , urlUpdate=urlUpdate
        }
      )
-}
programExBuilderWithFlagsAndNavigation :
    { init : flags -> navParseResult -> ( userModel, Cmd userMsg )
    , filters : userMsg -> userModel -> ( userMsg, States userModel userMsg )
    , update : userMsg -> userModel -> ( userModel, Cmd userMsg )
    , view : userModel -> Html.Html userMsg
    , subscriptions : userModel -> Sub userMsg
    , urlUpdate : navData -> userModel -> ( userModel, Cmd userMsg )
    }
    -> { init : flags -> navParseResult -> ( Model userModel userMsg, Cmd userMsg )
       , update : userMsg -> Model userModel userMsg -> ( Model userModel userMsg, Cmd userMsg )
       , view : Model userModel userMsg -> Html.Html userMsg
       , subscriptions : Model userModel userMsg -> Sub userMsg
       , urlUpdate : navData -> Model userModel userMsg -> ( Model userModel userMsg, Cmd userMsg )
       }
programExBuilderWithFlagsAndNavigation app =
    { init = flagsNavInit app
    , update = update app.filters app.update app.view app.subscriptions
    , view = view
    , subscriptions = subscriptions
    , urlUpdate = urlUpdate app.urlUpdate
    }
