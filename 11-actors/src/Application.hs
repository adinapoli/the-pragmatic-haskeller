{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Pragmatic.Snaplet.Actors

------------------------------------------------------------------------------
data App = App
    { _actors :: Snaplet (ActorSystem App)
    }

makeLenses ''App

instance HasActorSystem App where
  system app = undefined


------------------------------------------------------------------------------
type AppHandler = Handler App App


