{-# LANGUAGE OverloadedStrings #-}

module Pragmatic.Types (
  UserId,
  Auth(..)) where


import Data.Text
import Data.UUID


type UserId = Text

data Auth = Auth
    { userId :: UserId
    , token :: UUID }

