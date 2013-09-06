{-# LANGUAGE OverloadedStrings #-}

module Pragmatic.Snaplet.Actors where

import Data.Configurator.Types
import Control.Distributed.Process
import Snap.Snaplet

data ActorSystem a = ActorSystem
  { actorConfig :: Config
  , actorPool :: Process () }


actorSystemInit :: SnapletInit b v
actorSystemInit = undefined


class HasActorSystem a where
  system :: a -> Process ()
