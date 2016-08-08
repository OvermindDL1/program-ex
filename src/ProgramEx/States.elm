module ProgramEx.States exposing
  ( States
  , default
  , doUpdate
  , doSubscriptions
  , doView
  )

{-|
@docs States, default, doUpdate, doSubscriptions, doView
-}


{-| Holds the State information
-}
type States =
  S { doUpdate : Bool
    , doSubscriptions : Bool
    , doView : Bool
    }



{-| Gets the default state set, everything is allowed
-}
default : States
default =
  S { doUpdate=True
    , doSubscriptions=True
    , doView=True
    }

{-| Gets from the state if the update callback should be run
-}
doUpdate : States -> Bool
doUpdate (S states) = states.doUpdate


{-| Gets from the state if the subscriptions callback should be run
-}
doSubscriptions : States -> Bool
doSubscriptions (S states) = states.doSubscriptions


{-| Gets from the state if the view callback should be run
-}
doView : States -> Bool
doView (S states) = states.doView
