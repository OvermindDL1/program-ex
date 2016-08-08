module ProgramEx exposing (programExBuilderWithFlags, programExBuilderWithFlagsAndNavigation)


{-|
@docs programExBuilderWithFlags, programExBuilderWithFlagsAndNavigation
-}

import ProgramEx.States as States exposing (States, doUpdate, doSubscriptions, doView)

import Html



type alias Model userModel userMsg =
  { states : States
  , userModel : userModel
  , view : Html.Html userMsg
  , subscriptions : Sub userMsg
  }


flagsInit : ( flags -> (userModel, Cmd userMsg) ) -> flags -> ( Model userModel userMsg, Cmd userMsg )
flagsInit userInit flags =
  let
    -- stateInit : ( userModel, Cmd userMsg ) -- Broken in Elm right now due to annotation names not being shared in outer/inner scopes
    stateInit = userInit flags

    ( initModel, initCmd ) = stateInit
  in
    (
      { states=States.default
      , userModel=initModel
      , view=Html.div [] []
      , subscriptions=Sub.none
      }
    , initCmd
    )


update :
  ( userMsg -> userModel -> ( userMsg, States ) )
  -> ( userMsg -> userModel -> ( userModel, Cmd userMsg ) )
  -> ( userModel -> Html.Html userMsg)
  -> ( userModel -> Sub userMsg )
  -> userMsg
  -> Model userModel userMsg
  -> ( Model userModel userMsg, Cmd userMsg )
update userFilters userUpdate userView userSubscriptions msg model =
  let
    -- stateFiltered : ( userMsg, States ) -- Broken in Elm right now due to annotation names not being shared in outer/inner scopes
    stateFiltered =
      userFilters msg model.userModel

    ( transMsg, states ) = stateFiltered

    -- updateState : ( userModel, Cmd userMsg ) -- Broken in Elm right now due to annotation names not being shared in outer/inner scopes
    updateState =
      if doUpdate states then
        userUpdate msg model.userModel
      else
        ( model.userModel, Cmd.none )

    ( updateModel, updateCmd ) = updateState

    -- viewData : Html.Html userMsg -- Broken in Elm right now due to annotation names not being shared in outer/inner scopes
    viewData =
      if doView states then
        userView updateModel
      else
        model.view

    -- subscriptionsData : Sub userMsg -- Broken in Elm right now due to annotation names not being shared in outer/inner scopes
    subscriptionsData =
      if doSubscriptions states then
        userSubscriptions updateModel
      else
        model.subscriptions

  in
    if doUpdate states then
        ( { model
          | states=states
          , userModel=updateModel
          , view=viewData
          , subscriptions=subscriptionsData
          }
        , updateCmd
        )
    else
      ( model, Cmd.none )


view :Model userModel userMsg -> Html.Html userMsg
view model =
  model.view


subscriptions : Model userModel userMsg -> Sub userMsg
subscriptions model =
  model.subscriptions


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
programExBuilderWithFlags
    : { init : flags -> ( userModel, Cmd userMsg )
      , filters : userMsg -> userModel -> ( userMsg, States )
      , update : userMsg ->  userModel -> ( userModel, Cmd userMsg )
      , view : userModel -> Html.Html userMsg
      , subscriptions : userModel -> Sub userMsg
      }
    -> { init : flags -> ( Model userModel userMsg, Cmd userMsg )
      , update : userMsg -> Model userModel userMsg -> ( Model userModel userMsg, Cmd userMsg )
      , view : Model userModel userMsg -> Html.Html userMsg
      , subscriptions : Model userModel userMsg -> Sub userMsg
      }
programExBuilderWithFlags app =
      { init=flagsInit app.init
      , update=update app.filters app.update app.view app.subscriptions
      , view=view
      , subscriptions=subscriptions
      }



{-|
Pass in the usual callbacks and it returns a built callback set.

This can be used via Html.App.programWithFlags like:

    Navigation.programWithFlags urlParser
      { programExBuilderWithFlagsAndNavigation
        { init=init
        , filter=filter
        , update=update
        , view=view
        , subscriptions=subscriptions
        , urlUpdate=urlUpdate
        }
      }
-}
programExBuilderWithFlagsAndNavigation
    : { init : flags -> ( userModel, Cmd userMsg )
      , filters : userMsg -> userModel -> ( userMsg, States )
      , update : userMsg ->  userModel -> ( userModel, Cmd userMsg )
      , view : userModel -> Html.Html userMsg
      , subscriptions : userModel -> Sub userMsg
      , urlUpdate : data -> userModel -> (userModel, Cmd userMsg)
      }
    -> { init : flags -> ( Model userModel userMsg, Cmd userMsg )
      , update : userMsg -> Model userModel userMsg -> ( Model userModel userMsg, Cmd userMsg )
      , view : Model userModel userMsg -> Html.Html userMsg
      , subscriptions : Model userModel userMsg -> Sub userMsg
      , urlUpdate : data -> userModel -> (userModel, Cmd userMsg)
      }
programExBuilderWithFlagsAndNavigation app =
      { init=flagsInit app.init
      , update=update app.filters app.update app.view app.subscriptions
      , view=view
      , subscriptions=subscriptions
      , urlUpdate=app.urlUpdate
      }
